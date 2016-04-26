package com.wix.pay.paypal


import java.util.{Collections, Properties}

import com.paypal.api.openidconnect.{Error => PayPalIdentityError}
import com.paypal.api.payments.{CreditCard => PaypalCreditCard, _}
import com.paypal.base.SDKUtil
import com.paypal.base.rest.{OAuthTokenCredential, PayPalRESTException, PayPalResource}
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.creditcard.networks.Networks
import com.wix.pay.model.{CurrencyAmount, Name}
import com.wix.pay.{PaymentErrorException, PaymentException}
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

import scala.concurrent.duration.Duration
import scala.language.implicitConversions


class DefaultPaypalGatewayHelper(connectTimeout: Option[Duration] = None,
                                 readTimeout: Option[Duration] = None,
                                 numberOfRetries: Int = 0,
                                 maxConnections: Option[Int] = None,
                                 isGoogleAppEngine: Boolean = false,
                                 endpoint: String = "https://api.paypal.com") extends PaypalGatewayHelper {
  implicit val formats = DefaultFormats

  private def buildConfigurationMap(bnCode: Option[String] = None): java.util.Map[String, String]  = {
    val properties = new Properties()

    connectTimeout foreach (duration => properties.setProperty("http.ConnectionTimeOut", duration.toMillis.toString))
    readTimeout foreach (duration => properties.setProperty("http.ReadTimeOut", duration.toMillis.toString))
    properties.setProperty("http.Retry", numberOfRetries.toString)
    maxConnections foreach (n => properties.setProperty("http.MaxConnection", n.toString))

    properties.setProperty("http.GoogleAppEngine", isGoogleAppEngine.toString)
    properties.setProperty("service.EndPoint", endpoint)

    bnCode.foreach { properties.setProperty("PayPal-Partner-Attribution-Id", _) }

    PayPalResource.initConfig(properties)

    SDKUtil.constructMap(properties)
  }

  override def retrieveAccessToken(merchant: PaypalMerchant, bnCode: Option[String] = None): String = {
    def parseErrorJson(json: String): PayPalIdentityError = {
      val error = Serialization.read[com.wix.pay.paypal.model.Error](json)

      val e = new PayPalIdentityError()
      e.setError(error.error)
      e.setErrorDescription(error.error_description)
      e
    }
    def translatePaypalException(e: PayPalRESTException): PaymentException = {
      implicit class Regex(sc: StringContext) {
        def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
      }

      e.getMessage match {
        case r"Error code : (\d+)$statusCode with response : (\{.*\})$json" if statusCode == "401" =>
          val error = parseErrorJson(json)

          (error.getError, error.getErrorDescription) match {
            case ("invalid_client", "Client Authentication failed") =>
              new PaymentErrorException("PayPal Incorrect Client (merchant) Credentials", e)

            case _ =>
              new PaymentErrorException(e.getMessage, e)
          }

        case _ => new PaymentErrorException(e.getMessage, e)
      }
    }


    try {
      new OAuthTokenCredential(merchant.clientId, merchant.secret, buildConfigurationMap(bnCode)).getAccessToken
    } catch {
      case e: PayPalRESTException => throw translatePaypalException(e)
      case e: RuntimeException => throw e
    }
  }

  private def createPayment(intent: String, creditCard: CreditCard, currencyAmount: CurrencyAmount): Payment = {
    val transaction = new Transaction()
    transaction.setAmount(currencyAmount)

    val payment = new Payment(intent, createPayer(creditCard))
    payment.setTransactions(Collections.singletonList(transaction))
    payment
  }

  override def createAuthorize(creditCard: CreditCard, currencyAmount: CurrencyAmount): Payment = {
    createPayment("authorize", creditCard, currencyAmount)
  }

  override def createSale(creditCard: CreditCard, currencyAmount: CurrencyAmount): Payment = {
    createPayment("sale", creditCard, currencyAmount)
  }

  override def submitPayment(accessToken: String, payment: Payment): Payment = {
    payment.create(accessToken)
  }

  override def createCapture(currencyAmount: CurrencyAmount): Capture = {
    val capture = new Capture()

    capture.setIsFinalCapture(true)
    capture.setAmount(currencyAmount)

    capture
  }

  override def submitCapture(accessToken: String, authorizationId: String, capture: Capture): Capture = {
    val authorization = Authorization.get(accessToken, authorizationId)
    authorization.capture(accessToken, capture)
  }

  override def submitVoidAuthorization(accessToken: String, authorizationId: String): Authorization = {
    val authorization = Authorization.get(accessToken, authorizationId)
    authorization.doVoid(accessToken)
  }

  private def createPayer(creditCard: CreditCard): Payer = {
    val payer = new Payer()
    payer.setPaymentMethod("credit_card")

    val fundingInstrument = new FundingInstrument
    fundingInstrument.setCreditCard(creditCard)
    payer.setFundingInstruments(Collections.singletonList(fundingInstrument))

    payer
  }

  private implicit def toAmount(currencyAmount: CurrencyAmount): Amount = {
    new Amount(currencyAmount.currency, PaypalHelper.toPaypalAmount(currencyAmount.amount))
  }

  private implicit def toPaypalCreditCard(creditCard: CreditCard): PaypalCreditCard = {
    val ppCard = new PaypalCreditCard

    ppCard.setNumber(creditCard.number)

    Networks(creditCard.number) match {
      case None =>
        throw new PaymentException(s"Invalid credit card network: (${creditCard.number.substring(0, 6)}, ${creditCard.number.length})")
      case Some(network) => ppCard.setType(toPaypalCardType(network))
    }

    ppCard.setExpireYear(creditCard.expiration.year)
    ppCard.setExpireMonth(creditCard.expiration.month)

    creditCard.csc foreach (csc => ppCard.setCvv2(csc.toInt))
    creditCard.holderName foreach { n =>
      val name = splitName(n)
      ppCard.setFirstName(name.first)
      ppCard.setLastName(name.last)
    }

    // PayPal requires all address fields to be filled-in if an address is given, which unfortunately we don't have
    // @see https://developer.paypal.com/docs/api/#address-object
    /*
    val address = new Address
    creditCard.billingAddress foreach address.setLine1
    creditCard.billingPostalCode foreach address.setPostalCode
    ppCard.setBillingAddress(address)
    */

    ppCard
  }

  private def toPaypalCardType(cardType: String): String = {
    // @see https://developer.paypal.com/webapps/developer/docs/api/#store-a-credit-card
    cardType match {
      case Networks.visa => "visa"
      case Networks.masterCard => "mastercard"
      case Networks.discover => "discover"
      case Networks.amex => "amex"
      case _ => throw new PaymentException("Unsupported credit card type: " + cardType);
    }
  }

  private def splitName(name: String) = {
    val parts = name.split("\\s")
    parts match {
      case arr if arr.length == 1 => new Name(parts(0), "-")
      case _ => new Name(parts(0), parts.drop(1).mkString(" "))
    }
  }
}
