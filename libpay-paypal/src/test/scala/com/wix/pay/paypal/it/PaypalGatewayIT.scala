package com.wix.pay.paypal.it


import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.CurrencyAmount
import com.wix.pay.paypal._
import com.wix.pay.paypal.testkit.{PaypalDriver, PaypalError}
import com.wix.pay.{PaymentErrorException, PaymentGateway, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope
import spray.http.StatusCodes


class PaypalGatewayIT extends SpecWithJUnit {
  val paypalPort = 9033

  val driver = new PaypalDriver(port = paypalPort)
  val merchantParser = new JsonPaypalMerchantParser()
  val authorizationParser = new JsonPaypalAuthorizationParser()

  val someBnCode = "someBnCode"

  val someMerchant = new PaypalMerchant("some client ID", "some secret")
  val someMerchantKey = merchantParser.stringify(someMerchant)
  val someCurrencyAmount = CurrencyAmount("USD", 33.3)
  val someCreditCard = CreditCard(
    "4012888818888",
    YearMonth(2020, 12),
    additionalFields = Some(CreditCardOptionalFields.withFields(
      csc = Some("123"),
      holderId = Some("some holder id"),
      holderName = Some("some holder name"))))
  val someAccessToken = "someAccessToken"

  step {
    driver.startProbe()
  }

  sequential

  trait Ctx extends Scope {
    val paypal: PaymentGateway = new PaypalGateway(
      merchantParser = merchantParser,
      authorizationParser = authorizationParser,
      helper = new DefaultPaypalGatewayHelper(endpoint = s"http://localhost:$paypalPort"),
      bnCode = Some(someBnCode)
    )

    driver.resetProbe()
  }

  "sale request via PayPal gateway" should {
    "gracefully fail on rejected card" in new Ctx {
      driver.anAuthenticateRequestFor(
        someMerchant.clientId, someMerchant.secret
      ) returns someAccessToken
      driver.aSaleRequestFor(
        someAccessToken, someCurrencyAmount, someCreditCard
      ) errors(
        StatusCodes.BadRequest, PaypalError("CREDIT_CARD_REFUSED", "Credit card was refused")
      )

      paypal.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }
  }


  "authorize request via PayPal gateway" should {
    "gracefully fail on invalid merchant key" in new Ctx {
      driver.anAuthenticateRequestFor(someMerchant.clientId, someMerchant.secret) errors(
        StatusCodes.Unauthorized, PaypalError("invalid_client", "Invalid client credentials"))

      paypal.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentErrorException]
      )
    }

    "successfully yield an authorization key on valid request" in new Ctx {
      val someAuthorizationId = "some authorization ID"
      val someAuthorizationKey = authorizationParser.stringify(
        PaypalAuthorization(someAuthorizationId, someCurrencyAmount.currency))

      driver.anAuthenticateRequestFor(someMerchant.clientId, someMerchant.secret) returns someAccessToken
      driver.anAuthorizeRequestFor(someAccessToken, someCurrencyAmount, someCreditCard) returns someAuthorizationId

      paypal.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount
      ) must beASuccessfulTry(
        check = ===(someAuthorizationKey)
      )
    }

    "gracefully fail on rejected card" in new Ctx {
      driver.anAuthenticateRequestFor(someMerchant.clientId, someMerchant.secret) returns someAccessToken
      driver.anAuthorizeRequestFor(someAccessToken, someCurrencyAmount, someCreditCard) errors(
        StatusCodes.BadRequest, PaypalError("CREDIT_CARD_REFUSED", "Credit card was refused"))

      paypal.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        currencyAmount = someCurrencyAmount
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }
  }


  step {
    driver.stopProbe()
  }
}
