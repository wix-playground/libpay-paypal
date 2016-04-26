package com.wix.pay.paypal


import java.util.Collections

import com.paypal.api.payments._
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.CurrencyAmount
import org.specs2.matcher._
import org.specs2.mock.Mockito
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope


class PaypalGatewayTest extends SpecWithJUnit with Mockito {
  trait Ctx extends Scope {
    val merchantParser = new JsonPaypalMerchantParser
    val authorizationParser = new JsonPaypalAuthorizationParser
    val helper = mock[PaypalGatewayHelper]

    val someBnCode = "someBnCode"

    val paypalGateway = new PaypalGateway(
      helper = helper,
      merchantParser = merchantParser,
      authorizationParser = authorizationParser)

    val paypalGatewayWithBnCode = new PaypalGateway(
      helper = helper,
      merchantParser = merchantParser,
      authorizationParser = authorizationParser,
      bnCode = Some(someBnCode))

    val someMerchant = PaypalMerchant("some client ID", "some secret")
    val someMerchantKey = merchantParser.stringify(someMerchant)
    val someCurrencyAmount = CurrencyAmount("USD", 33.3)
    val someCreditCard = CreditCard(
      "4012888818888",
      YearMonth(2016, 12),
      additionalFields = Some(CreditCardOptionalFields.withFields(
        csc = Some("123"),
        holderId = Some("some holder id"),
        holderName = Some("some holder name"))))
    val somePayment = new Payment(null, null)
    val someCapture = new Capture()
    val someAccessToken = "some access token"

    val someAuthorizationId = "some authorization ID"
    val someAuthorizationKey = authorizationParser.stringify(PaypalAuthorization(someAuthorizationId, someCurrencyAmount.currency))

    val someAuthorizedPayment = new Payment()
    val someAuthorizedPaymentTransaction = new Transaction()
    val someAuthorizedPaymentTransactionRelatedResources = new RelatedResources()
    val someAuthorizedPaymentTransactionRelatedResourcesAuthorization = new Authorization()
    someAuthorizedPaymentTransactionRelatedResourcesAuthorization.setId(someAuthorizationId)
    someAuthorizedPaymentTransactionRelatedResources.setAuthorization(someAuthorizedPaymentTransactionRelatedResourcesAuthorization)
    someAuthorizedPaymentTransaction.setRelatedResources(Collections.singletonList(someAuthorizedPaymentTransactionRelatedResources))
    someAuthorizedPayment.setTransactions(Collections.singletonList(someAuthorizedPaymentTransaction))

    val someSoldId = "some sold ID"
    val someSoldPayment = new Payment()
    someSoldPayment.setId(someSoldId)

    val someVoidedAuthorizationId = "some voided authorization ID"
    val someVoidedAuthorization = new Authorization()
    someVoidedAuthorization.setId(someVoidedAuthorizationId)

    val someCapturedId = "some captured transaction ID"
    val someCaptured = new Capture()
    someCaptured.setId(someCapturedId)

    val validateHelperAuthorizeOrSaleFlow: (Option[String], Unit => Payment) => MatchResult[Payment] = (bnCode, f) => {
      got {
        one(helper).retrieveAccessToken(someMerchant, bnCode)
        f.apply(Unit)
        one(helper).submitPayment(someAccessToken, somePayment)
      }
    }

    val validateHelperCaptureFlow: (Option[String], Unit => Capture) => MatchResult[Capture] = (bnCode, f) => {
      got {
        one(helper).retrieveAccessToken(someMerchant, bnCode)
        f.apply(Unit)
        one(helper).submitCapture(someAccessToken, someAuthorizationId, someCapture)
      }
    }

    def validateHelperVoidAuthorizationFlow = (bnCode: Option[String]) => {
      got {
        one(helper).retrieveAccessToken(someMerchant, bnCode)
        one(helper).submitVoidAuthorization(someAccessToken, someAuthorizationId)
      }
    }
  }

  "authorize" should {
    "retrieve an access token without BN code, create an authorization payment, submit it and return the authorization ID if all OK" in new Ctx {
      helper.retrieveAccessToken(someMerchant, None) returns
        someAccessToken

      helper.createAuthorize(someCreditCard, someCurrencyAmount) returns
        somePayment

      helper.submitPayment(someAccessToken, somePayment) returns
        someAuthorizedPayment

      paypalGateway.authorize(someMerchantKey, someCreditCard, someCurrencyAmount) must
        beASuccessfulTry(check = ===(someAuthorizationKey))

      validateHelperAuthorizeOrSaleFlow(None, _ => one(helper)
        .createAuthorize(someCreditCard, someCurrencyAmount))
    }

    "retrieve an access token with BN code, create an authorization payment, submit it and return the authorization ID if all OK" in new Ctx {
      helper.retrieveAccessToken(someMerchant, Some(someBnCode)) returns
        someAccessToken

      helper.createAuthorize(someCreditCard, someCurrencyAmount) returns
        somePayment

      helper.submitPayment(someAccessToken, somePayment) returns
        someAuthorizedPayment

      paypalGatewayWithBnCode.authorize(someMerchantKey, someCreditCard, someCurrencyAmount) must
        beASuccessfulTry(check = ===(someAuthorizationKey))

      validateHelperAuthorizeOrSaleFlow(Some(someBnCode), _ => one(helper)
        .createAuthorize(someCreditCard, someCurrencyAmount))
    }
  }

  "capture" should {
    "retrieve an access token without BN code, create a capture, submit it and return the transaction ID if all OK" in new Ctx {
      helper.retrieveAccessToken(someMerchant, None) returns
        someAccessToken

      helper.createCapture(someCurrencyAmount) returns
        someCapture

      helper.submitCapture(someAccessToken, someAuthorizationId, someCapture) returns
        someCaptured

      paypalGateway.capture(someMerchantKey, someAuthorizationKey, someCurrencyAmount.amount) must
        beASuccessfulTry(check = ===(someCapturedId))

      validateHelperCaptureFlow(None, _ => one(helper)
        .createCapture(someCurrencyAmount))
    }

    "retrieve an access token with BN code, create a capture, submit it and return the transaction ID if all OK" in new Ctx {
      helper.retrieveAccessToken(someMerchant, Some(someBnCode)) returns
        someAccessToken

      helper.createCapture(someCurrencyAmount) returns
        someCapture

      helper.submitCapture(someAccessToken, someAuthorizationId, someCapture) returns
        someCaptured

      paypalGatewayWithBnCode.capture(someMerchantKey, someAuthorizationKey, someCurrencyAmount.amount) must
        beASuccessfulTry(check = ===(someCapturedId))

      validateHelperCaptureFlow(Some(someBnCode), _ => one(helper)
        .createCapture(someCurrencyAmount))
    }
  }

  "sale" should {
    "retrieve an access token without BN code, create a sale payment, submit it and return the transaction ID if all OK" in new Ctx {
      helper.retrieveAccessToken(someMerchant, None) returns
        someAccessToken

      helper.createSale(someCreditCard, someCurrencyAmount) returns
        somePayment

      helper.submitPayment(someAccessToken, somePayment) returns
        someSoldPayment

      paypalGateway.sale(someMerchantKey, someCreditCard, someCurrencyAmount) must
        beASuccessfulTry(check = ===(someSoldId))

      validateHelperAuthorizeOrSaleFlow(None, _ => one(helper)
        .createSale(someCreditCard, someCurrencyAmount))
    }

    "retrieve an access token with BN code, create a sale payment, submit it and return the transaction ID if all OK" in new Ctx {
      helper.retrieveAccessToken(someMerchant, Some(someBnCode)) returns
        someAccessToken

      helper.createSale(someCreditCard, someCurrencyAmount) returns
        somePayment

      helper.submitPayment(someAccessToken, somePayment) returns
        someSoldPayment

      paypalGatewayWithBnCode.sale(someMerchantKey, someCreditCard, someCurrencyAmount) must
        beASuccessfulTry(check = ===(someSoldId))

      validateHelperAuthorizeOrSaleFlow(Some(someBnCode), _ => one(helper)
        .createSale(someCreditCard, someCurrencyAmount))
    }
  }

  "voidAuthorization" should {
    "retrieve an access token without BN code, submit the authorization void and return the transaction ID if all OK" in new Ctx {
      helper.retrieveAccessToken(someMerchant, None) returns
        someAccessToken

      helper.submitVoidAuthorization(someAccessToken, someAuthorizationId) returns
        someVoidedAuthorization

      paypalGateway.voidAuthorization(someMerchantKey, someAuthorizationKey) must
        beASuccessfulTry(check = ===(someVoidedAuthorizationId))

      validateHelperVoidAuthorizationFlow(None)
    }

    "retrieve an access token with BN code, submit the authorization void and return the transaction ID if all OK" in new Ctx {
      helper.retrieveAccessToken(someMerchant, Some(someBnCode)) returns
        someAccessToken

      helper.submitVoidAuthorization(someAccessToken, someAuthorizationId) returns
        someVoidedAuthorization

      paypalGatewayWithBnCode.voidAuthorization(someMerchantKey, someAuthorizationKey) must
        beASuccessfulTry(check = ===(someVoidedAuthorizationId))

      validateHelperVoidAuthorizationFlow(Some(someBnCode))
    }
  }
}
