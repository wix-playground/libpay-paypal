package com.wix.pay.paypal


import com.paypal.api.payments.Transaction
import com.paypal.base.rest.PayPalRESTException
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.{CurrencyAmount, Customer, Deal, Payment}
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentGateway}

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}


/**
  * @param rejectOnUnknownErrors   If true, unknown errors are treated as payment rejection.
  *                                Otherwise, they are treated as general payment errors.
  *
  * @note When a card number is flagged by PayPal's fraud detection mechanism, transactions that use it may start
  *       failing on "unknown errors". For example, this is the case for the test card 4111111111111111.
  *       To workaround this, set rejectOnUnknownErrors to true.
  * @see <a href="https://www.paypal-knowledge.com/infocenter/index?page=content&id=FAQ1041">What is API error code 10001?</a>
  */
class PaypalGateway(helper: PaypalGatewayHelper = new DefaultPaypalGatewayHelper,
                    merchantParser: PaypalMerchantParser = new JsonPaypalMerchantParser,
                    authorizationParser: PaypalAuthorizationParser = new JsonPaypalAuthorizationParser,
                    bnCode: Option[String] = None,
                    rejectOnUnknownErrors: Boolean = false) extends PaymentGateway {
  private val exceptionsTranslator = new ExceptionsTranslator(
    rejectOnUnknownErrors = rejectOnUnknownErrors
  )

  override def authorize(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      require(payment.installments == 1, "PayPal does not support installments")

      val merchant = merchantParser.parse(merchantKey)
      val accessToken = helper.retrieveAccessToken(merchant, bnCode)

      val authorize = helper.createAuthorize(creditCard, payment.currencyAmount)
      val authorized = helper.submitPayment(accessToken, authorize)

      val authorizationId = getAuthorizationId(authorized.getTransactions.get(0))
      authorizationParser.stringify(PaypalAuthorization(authorizationId, payment.currencyAmount.currency))
    } match {
      case Success(authorizationKey) => Success(authorizationKey)
      case Failure(e: PayPalRESTException) => Failure(exceptionsTranslator.translatePaypalException(e))
      case Failure(e) => Failure(PaymentErrorException(e.getMessage, e))
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
      case Failure(e: PayPalRESTException) => Failure(exceptionsTranslator.translatePaypalException(e))
      case Failure(e) => Failure(PaymentErrorException(e.getMessage, e))
    }
  }

  override def sale(merchantKey: String, creditCard: CreditCard, payment: Payment, customer: Option[Customer], deal: Option[Deal]): Try[String] = {
    Try {
      require(payment.installments == 1, "PayPal does not support installments")

      val merchant = merchantParser.parse(merchantKey)
      val accessToken = helper.retrieveAccessToken(merchant, bnCode)

      val sale = helper.createSale(creditCard, payment.currencyAmount)
      val sold = helper.submitPayment(accessToken, sale)

      sold.getId
    } match {
      case Success(transactionId) => Success(transactionId)
      case Failure(e: PayPalRESTException) => Failure(exceptionsTranslator.translatePaypalException(e))
      case Failure(e) => Failure(PaymentErrorException(e.getMessage, e))
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
      case Failure(e: PayPalRESTException) => Failure(exceptionsTranslator.translatePaypalException(e))
      case Failure(e) => Failure(PaymentErrorException(e.getMessage, e))
    }
  }

  private def getAuthorizationId(transaction: Transaction): String = {
    transaction.getRelatedResources.find { resource =>
      Option(resource.getAuthorization).isDefined
    } match {
      case Some(authorizationResource) => authorizationResource.getAuthorization.getId
      case None => throw PaymentException("Invalid authorization ID")
    }
  }
}
