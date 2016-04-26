package com.wix.pay.paypal.testkit


import com.wix.hoopoe.http.testkit.EmbeddedHttpProbe
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.CurrencyAmount
import com.wix.pay.paypal.PaypalHelper
import com.wix.pay.paypal.model.ErrorNames
import spray.http._


class PaypalDriver(port: Int) {
  private val probe = new EmbeddedHttpProbe(port, EmbeddedHttpProbe.NotFoundHandler)

  def startProbe() {
    probe.doStart()
  }

  def stopProbe() {
    probe.doStop()
  }

  def resetProbe() {
    probe.handlers.clear()
  }

  def anAuthenticateRequestFor(clientId: String, secret: String): AuthenticateCtx = {
    new AuthenticateCtx(clientId, secret)
  }

  def aSaleRequestFor(accessToken: String,
                      currencyAmount: CurrencyAmount,
                      card: CreditCard): SaleCtx = {
    new SaleCtx(accessToken, currencyAmount, card)
  }

  def anAuthorizeRequestFor(accessToken: String,
                            currencyAmount: CurrencyAmount,
                            card: CreditCard): AuthorizeCtx = {
    new AuthorizeCtx(accessToken, currencyAmount, card)
  }

  abstract class Ctx(val resource: String) {
    /** Verifies that the specified HTTP Entity matches the stubbed request. */
    def isStubbedRequestEntity(entity: HttpEntity): Boolean

    def errors(statusCode: StatusCode, errorName: String, errorMessage: String): Unit = {
      probe.handlers += {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`resource`),
        _,
        entity,
        _) if isStubbedRequestEntity(entity) =>
          HttpResponse(
            status = statusCode,
            entity = HttpEntity(ContentTypes.`application/json`,
              "{\n  \"name\": \"" + errorName + "\",\n  \"message\": \"" + errorMessage + "\"\n}"))
      }
    }

    def isRefused(): Unit = {
      errors(
        statusCode = StatusCodes.BadRequest,
        errorName = ErrorNames.creditCardRefused,
        errorMessage = "Credit card was refused"
      )
    }

    def failsOnWrongCredentials(): Unit = {
      errors(
        statusCode = StatusCodes.Unauthorized,
        errorName = ErrorNames.invalidClient,
        errorMessage = "Invalid client credentials"
      )
    }

    def failsCscCheck(): Unit = {
      errors(
        statusCode = StatusCodes.Unauthorized,
        errorName = ErrorNames.creditCardCvvCheckFailed,
        errorMessage = "The credit card CVV check failed"
      )
    }
  }

  class AuthenticateCtx(clientId: String, secret: String) extends Ctx("/v1/oauth2/token") {
    def returns(accessToken: String): Unit = {
      probe.handlers += {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`resource`),
        _,
        entity,
        _) if isStubbedRequestEntity(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(
              ContentTypes.`application/json`,
              "{\n  \"scope\": \"https://api.paypal.com/v1/payments/.* https://api.paypal.com/v1/vault/credit-card https://api.paypal.com/v1/vault/credit-card/.*\",\n  \"access_token\": \"" + accessToken + "\",\n  \"token_type\": \"Bearer\",\n  \"app_id\": \"APP-6XR95014BA15863X\",\n  \"expires_in\": 28800\n}"))
      }
    }

    override def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      // TODO: verify clientId, secret
      true
    }
  }

  private def saleOrAuthorizeJson(currencyAmount: CurrencyAmount, transactionId: String, authorizationId: String): String = {
    val amountStr = PaypalHelper.toPaypalAmount(currencyAmount.amount)

    "{\n  \"id\": \"" + transactionId + "\",\n  \"create_time\": \"2013-01-31T04:12:02Z\",\n  \"update_time\": \"2013-01-31T04:12:04Z\",\n  \"state\": \"approved\",\n  \"intent\": \"authorize\",\n  \"payer\": {\n    \"payment_method\": \"credit_card\",\n    \"funding_instruments\": [\n      {\n        \"credit_card\": {\n          \"type\": \"visa\",\n          \"number\": \"xxxxxxxxxxxx0331\",\n          \"expire_month\": \"11\",\n          \"expire_year\": \"2018\",\n          \"first_name\": \"Betsy\",\n          \"last_name\": \"Buyer\",\n          \"billing_address\": {\n            \"line1\": \"111 First Street\",\n            \"city\": \"Saratoga\",\n            \"state\": \"CA\",\n            \"postal_code\": \"95070\",\n            \"country_code\": \"US\"\n          }\n        }\n      }\n    ]\n  },\n  \"transactions\": [\n    {\n      \"amount\": {\n        \"total\": \"" +
      amountStr + "\",\n        \"currency\": \"" +
      currencyAmount.currency + "\",\n        \"details\": {\n          \"tax\": \"0.00\",\n          \"shipping\": \"0.00\"\n        }\n      },\n      \"description\": \"This is the payment transaction description.\",\n      \"related_resources\": [\n        {\n          \"authorization\": {\n            \"id\": \"" +
      authorizationId + "\",\n            \"create_time\": \"2013-01-31T04:12:02Z\",\n            \"update_time\": \"2013-01-31T04:12:04Z\",\n            \"state\": \"completed\",\n            \"amount\": {\n              \"total\": \"" +
      amountStr + "\",\n              \"currency\": \"" + currencyAmount.currency + "\"\n            },\n            \"parent_payment\": \"" + transactionId + "\",\n            \"links\": [\n              {\n                \"href\": \"https://api.sandbox.paypal.com/v1/payments/sale/4RR959492F879224U\",\n                \"rel\": \"self\",\n                \"method\": \"GET\"\n              },\n              {\n                \"href\": \"https://api.sandbox.paypal.com/v1/payments/sale/4RR959492F879224U/refund\",\n                \"rel\": \"refund\",\n                \"method\": \"POST\"\n              },\n              {\n                \"href\": \"https://api.sandbox.paypal.com/v1/payments/payment/" + transactionId + "\",\n                \"rel\": \"parent_payment\",\n                \"method\": \"GET\"\n              }\n            ]\n          }\n        }\n      ]\n    }\n  ],\n  \"links\": [\n    {\n      \"href\": \"https://api.sandbox.paypal.com/v1/payments/payment/" + transactionId + "\",\n      \"rel\": \"self\",\n      \"method\": \"GET\"\n    }\n  ]\n}"
  }

  class SaleCtx(accessToken: String,
                currencyAmount: CurrencyAmount,
                card: CreditCard) extends Ctx("/v1/payments/payment") {

    def returns(transactionId: String): Unit = {
      probe.handlers += {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`resource`),
        _,
        entity,
        _) if isStubbedRequestEntity(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(ContentTypes.`application/json`, saleOrAuthorizeJson(currencyAmount, transactionId, "xxx")))
      }
    }

    override def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      // TODO: verify accessToken, currencyAmount. card
      true
    }
  }

  class AuthorizeCtx(accessToken: String,
                     currencyAmount: CurrencyAmount,
                     card: CreditCard) extends Ctx("/v1/payments/payment") {

    def returns(authorizationId: String): Unit = {
      val amountStr = PaypalHelper.toPaypalAmount(currencyAmount.amount)
      probe.handlers += {
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path(`resource`),
        _,
        entity,
        _) if isStubbedRequestEntity(entity) =>
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity(ContentTypes.`application/json`, saleOrAuthorizeJson(currencyAmount, "xxx", authorizationId)))
      }
    }

    override def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      // TODO: verify accessToken, currencyAmount. card
      true
    }
  }
}
