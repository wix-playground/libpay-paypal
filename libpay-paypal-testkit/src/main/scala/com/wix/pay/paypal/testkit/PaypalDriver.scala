package com.wix.pay.paypal.testkit


import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model._
import com.wix.e2e.http.api.StubWebServer
import com.wix.e2e.http.server.WebServerFactory.aStubWebServer
import com.wix.pay.creditcard.CreditCard
import com.wix.pay.model.CurrencyAmount
import com.wix.pay.paypal.PaypalHelper
import com.wix.pay.paypal.model.ErrorNames


class PaypalDriver(port: Int) {
  private val server: StubWebServer = aStubWebServer.onPort(port).build

  def start(): Unit = server.start()

  def stop(): Unit = server.stop()

  def reset(): Unit = server.replaceWith()


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
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path(`resource`),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = statusCode,
              entity = HttpEntity(
                ContentTypes.`application/json`,
                s"""{
                   | "name": "$errorName",
                   | "message": "$errorMessage"
                   |}""".stripMargin))
      }
    }

    def getsRefused(): Unit = {
      errors(
        statusCode = StatusCodes.BadRequest,
        errorName = ErrorNames.creditCardRefused,
        errorMessage = "Credit card was refused")
    }

    def failsOnWrongCredentials(): Unit = {
      errors(
        statusCode = StatusCodes.Unauthorized,
        errorName = ErrorNames.invalidClient,
        errorMessage = "Invalid client credentials")
    }

    def failsCscCheck(): Unit = {
      errors(
        statusCode = StatusCodes.Unauthorized,
        errorName = ErrorNames.creditCardCvvCheckFailed,
        errorMessage = "The credit card CVV check failed")
    }
  }

  class AuthenticateCtx(clientId: String, secret: String) extends Ctx("/v1/oauth2/token") {
    def returns(accessToken: String): Unit = {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path(`resource`),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                ContentTypes.`application/json`,
                s"""{
                   | "scope": "https://api.paypal.com/v1/payments/.* https://api.paypal.com/v1/vault/credit-card https://api.paypal.com/v1/vault/credit-card/.*",
                   | "access_token": "$accessToken",
                   | "token_type": "Bearer",
                   | "app_id": "APP-6XR95014BA15863X",
                   | "expires_in": 28800
                   |}""".stripMargin))
      }
    }

    override def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      // TODO: verify clientId, secret
      true
    }
  }

  private def saleOrAuthorizeJson(currencyAmount: CurrencyAmount,
                                  transactionId: String,
                                  authorizationId: String): String = {
    val amountStr = PaypalHelper.toPaypalAmount(currencyAmount.amount)

    s"""{
       |  "id": "$transactionId",
       |  "create_time": "2013-01-31T04:12:02Z",
       |  "update_time": "2013-01-31T04:12:04Z",
       |  "state": "approved",
       |  "intent": "authorize",
       |  "payer": {
       |    "payment_method": "credit_card",
       |    "funding_instruments": [
       |      {
       |        "credit_card": {
       |          "type": "visa",
       |          "number": "xxxxxxxxxxxx0331",
       |          "expire_month": "11",
       |          "expire_year": "2018",
       |          "first_name": "Betsy",
       |          "last_name": "Buyer",
       |          "billing_address": {
       |            "line1": "111 First Street",
       |            "city": "Saratoga",
       |            "state": "CA",
       |            "postal_code": "95070",
       |            "country_code": "US"
       |          }
       |        }
       |      }
       |    ]
       |  },
       |  "transactions": [
       |    {
       |      "amount": {
       |        "total": "$amountStr",
       |        "currency": "${currencyAmount.currency}",
       |        "details": {
       |          "tax": "0.00",
       |          "shipping": "0.00"
       |        }
       |      },
       |      "description": "This is the payment transaction description.",
       |      "related_resources": [
       |        {
       |          "authorization": {
       |            "id": "$authorizationId",
       |            "create_time": "2013-01-31T04:12:02Z",
       |            "update_time": "2013-01-31T04:12:04Z",
       |            "state": "completed",
       |            "amount": {
       |              "total": "$amountStr",
       |              "currency": "${currencyAmount.currency}"
       |            },
       |            "parent_payment": "$transactionId",
       |            "links": [
       |              {
       |                "href": "https://api.sandbox.paypal.com/v1/payments/sale/4RR959492F879224U",
       |                "rel": "self",
       |                "method": "GET"
       |              },
       |              {
       |                "href": "https://api.sandbox.paypal.com/v1/payments/sale/4RR959492F879224U/refund",
       |                "rel": "refund",
       |                "method": "POST"
       |              },
       |              {
       |                "href": "https://api.sandbox.paypal.com/v1/payments/payment/$transactionId",
       |                "rel": "parent_payment",
       |                "method": "GET"
       |              }
       |            ]
       |          }
       |        }
       |      ]
       |    }
       |  ],
       |  "links": [
       |    {
       |      "href": "https://api.sandbox.paypal.com/v1/payments/payment/$transactionId",
       |      "rel": "self",
       |      "method": "GET"
       |    }
       |  ]
       |}""".stripMargin
  }

  class SaleCtx(accessToken: String,
                currencyAmount: CurrencyAmount,
                card: CreditCard) extends Ctx("/v1/payments/payment") {

    def returns(transactionId: String): Unit = {
      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path(`resource`),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                ContentTypes.`application/json`,
                saleOrAuthorizeJson(currencyAmount, transactionId, "xxx")))
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

      server.appendAll {
        case HttpRequest(
          HttpMethods.POST,
          Path(`resource`),
          _,
          entity,
          _) if isStubbedRequestEntity(entity) =>
            HttpResponse(
              status = StatusCodes.OK,
              entity = HttpEntity(
                ContentTypes.`application/json`,
                saleOrAuthorizeJson(currencyAmount, "xxx", authorizationId)))
      }
    }

    override def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      // TODO: verify accessToken, currencyAmount. card
      true
    }
  }
}
