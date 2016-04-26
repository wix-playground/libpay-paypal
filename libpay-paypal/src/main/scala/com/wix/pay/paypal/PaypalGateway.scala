package com.wix.pay.paypal


import com.paypal.api.payments.Transaction
import com.paypal.base.rest.PayPalRESTException
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{CurrencyAmount, Customer, Deal}
import com.wix.pay.paypal.model.ErrorNames
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway, PaymentRejectedException}

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}

class PaypalGateway(helper: PaypalGatewayHelper = new DefaultPaypalGatewayHelper,
                    merchantParser: PaypalMerchantParser = new JsonPaypalMerchantParser,
                    authorizationParser: PaypalAuthorizationParser = new JsonPaypalAuthorizationParser,
                    bnCode: Option[String] = None) extends PaymentGateway {

  override def authorize(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)
      val accessToken = helper.retrieveAccessToken(merchant, bnCode)

      val authorize = helper.createAuthorize(creditCard, currencyAmount)
      val authorized = helper.submitPayment(accessToken, authorize)

      val authorizationId = getAuthorizationId(authorized.getTransactions.get(0))
      authorizationParser.stringify(PaypalAuthorization(authorizationId, currencyAmount.currency))
    } match {
      case Success(authorizationKey) => Success(authorizationKey)
      case Failure(e: PayPalRESTException) => Failure(translatePaypalException(e))
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  override def capture(merchantKey: String, authorizationKey: String, amount: Double): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)
      val accessToken = helper.retrieveAccessToken(merchant, bnCode)
      val authorization = authorizationParser.parse(authorizationKey)

      val capture = helper.createCapture(CurrencyAmount(authorization.currency, amount))
      val captured = helper.submitCapture(accessToken, authorization.authorizationId, capture)

      captured.getId
    } match {
      case Success(transactionId) => Success(transactionId)
      case Failure(e: PayPalRESTException) => Failure(translatePaypalException(e))
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  override def sale(merchantKey: String, creditCard: CreditCard, currencyAmount: CurrencyAmount, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)
      val accessToken = helper.retrieveAccessToken(merchant, bnCode)

      val sale = helper.createSale(creditCard, currencyAmount)
      val sold = helper.submitPayment(accessToken, sale)

      sold.getId
    } match {
      case Success(transactionId) => Success(transactionId)
      case Failure(e: PayPalRESTException) => Failure(translatePaypalException(e))
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  override def voidAuthorization(merchantKey: String, authorizationKey: String): Try[String] = {
    Try {
      val merchant = merchantParser.parse(merchantKey)
      val accessToken = helper.retrieveAccessToken(merchant, bnCode)
      val authorization = authorizationParser.parse(authorizationKey)

      val ppAuthorization = helper.submitVoidAuthorization(accessToken, authorization.authorizationId)

      ppAuthorization.getId
    } match {
      case Success(transactionId) => Success(transactionId)
      case Failure(e: PayPalRESTException) => Failure(translatePaypalException(e))
      case Failure(e) => Failure(new PaymentErrorException(e.getMessage, e))
    }
  }

  private def getAuthorizationId(transaction: Transaction): String = {
    transaction.getRelatedResources.find { resource =>
      Option(resource.getAuthorization).isDefined
    } match {
      case Some(authorizationResource) => authorizationResource.getAuthorization.getId
      case None => throw new PaymentException("Invalid authorization ID")
    }
  }

  def translatePaypalException(e: PayPalRESTException): PaymentException = {
    implicit class Regex(sc: StringContext) {
      def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }

    Option(e.getDetails) match {
      case Some(error) => error.getName match {
        case ErrorNames.creditCardRefused|ErrorNames.creditCardCvvCheckFailed => new PaymentRejectedException(e.getMessage, e)
        case _ => new PaymentErrorException(e.getMessage, e)
      }
      case None => new PaymentErrorException(e.getMessage, e)
    }
  }
}
