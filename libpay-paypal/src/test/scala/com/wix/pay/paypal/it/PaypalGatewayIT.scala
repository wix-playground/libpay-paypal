package com.wix.pay.paypal.it


import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.{CurrencyAmount, Payment}
import com.wix.pay.paypal._
import com.wix.pay.paypal.testkit.PaypalDriver
import com.wix.pay.{PaymentErrorException, PaymentGateway, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope


class PaypalGatewayIT extends SpecWithJUnit {
  val paypalPort = 9033

  val driver = new PaypalDriver(port = paypalPort)
  val merchantParser = new JsonPaypalMerchantParser()
  val authorizationParser = new JsonPaypalAuthorizationParser()

  val someBnCode = "someBnCode"

  val someMerchant = new PaypalMerchant("some client ID", "some secret")
  val someMerchantKey = merchantParser.stringify(someMerchant)
  val someCurrencyAmount = CurrencyAmount("USD", 33.3)
  val somePayment = Payment(someCurrencyAmount, 1)
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
    "gracefully fail on invalid merchant key" in new Ctx {
      driver.anAuthenticateRequestFor(someMerchant.clientId, someMerchant.secret) failsOnWrongCredentials()

      paypal.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentErrorException]
      )
    }

    "successfully yield a transaction ID on valid request" in new Ctx {
      val someTransactionId = "some transaction ID"

      driver.anAuthenticateRequestFor(someMerchant.clientId, someMerchant.secret) returns someAccessToken
      driver.aSaleRequestFor(
        accessToken = someAccessToken,
        currencyAmount = someCurrencyAmount,
        card = someCreditCard
      ) returns someTransactionId

      paypal.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beASuccessfulTry(
        check = ===(someTransactionId)
      )
    }

    "gracefully fail on rejected card" in new Ctx {
      driver.anAuthenticateRequestFor(
        someMerchant.clientId, someMerchant.secret
      ) returns someAccessToken
      driver.aSaleRequestFor(
        accessToken = someAccessToken,
        currencyAmount = someCurrencyAmount,
        card = someCreditCard
      ) isRefused()

      paypal.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }

    "gracefully fail on CSC error" in new Ctx {
      driver.anAuthenticateRequestFor(
        someMerchant.clientId, someMerchant.secret
      ) returns someAccessToken
      driver.aSaleRequestFor(
        accessToken = someAccessToken,
        currencyAmount = someCurrencyAmount,
        card = someCreditCard
      ) failsCscCheck()

      paypal.sale(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }
  }

  "authorize request via PayPal gateway" should {
    "gracefully fail on invalid merchant key" in new Ctx {
      driver.anAuthenticateRequestFor(someMerchant.clientId, someMerchant.secret) failsOnWrongCredentials()

      paypal.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
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
        payment = somePayment
      ) must beASuccessfulTry(
        check = ===(someAuthorizationKey)
      )
    }

    "gracefully fail on rejected card" in new Ctx {
      driver.anAuthenticateRequestFor(someMerchant.clientId, someMerchant.secret) returns someAccessToken
      driver.anAuthorizeRequestFor(
        accessToken = someAccessToken,
        currencyAmount = someCurrencyAmount,
        card = someCreditCard
      ) isRefused()

      paypal.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }

    "gracefully fail on CSC error" in new Ctx {
      driver.anAuthenticateRequestFor(
        someMerchant.clientId, someMerchant.secret
      ) returns someAccessToken
      driver.anAuthorizeRequestFor(
        accessToken = someAccessToken,
        currencyAmount = someCurrencyAmount,
        card = someCreditCard
      ) failsCscCheck()

      paypal.authorize(
        merchantKey = someMerchantKey,
        creditCard = someCreditCard,
        payment = somePayment
      ) must beAFailedTry(
        check = beAnInstanceOf[PaymentRejectedException]
      )
    }
  }


  step {
    driver.stopProbe()
  }
}
