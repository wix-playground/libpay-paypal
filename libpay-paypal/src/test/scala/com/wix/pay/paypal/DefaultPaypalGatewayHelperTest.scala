package com.wix.pay.paypal


import com.paypal.api.payments.{Address, Amount, CreditCard => PaypalCreditCard, Payer, Payment}
import com.wix.pay.PaymentErrorException
import com.wix.pay.creditcard.{CreditCard, CreditCardOptionalFields, YearMonth}
import com.wix.pay.model.CurrencyAmount
import org.specs2.matcher.MustMatchers._
import org.specs2.matcher.{AlwaysMatcher, Matcher}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope


class DefaultPaypalGatewayHelperTest extends SpecWithJUnit {

  trait Ctx extends Scope {
    val helper = new DefaultPaypalGatewayHelper()
  }

  def bePayment(intent: Matcher[String] = AlwaysMatcher(),
                amount: Matcher[Amount] = AlwaysMatcher(),
                payer: Matcher[Payer] = AlwaysMatcher()): Matcher[Payment] = {
    intent ^^ { (_: Payment).getIntent aka "intent" } and
      amount ^^ { (_: Payment).getTransactions.get(0).getAmount aka "amount" } and
      payer ^^ { (_: Payment).getPayer aka "payer" }
  }

  def beAmount(currency: Matcher[String] = AlwaysMatcher(),
               total: Matcher[String] = AlwaysMatcher()): Matcher[Amount] = {
    currency ^^ { (_: Amount).getCurrency aka "currency" } and
      total ^^ { (_: Amount).getTotal aka "total" }
  }

  def bePayer(paymentMethod: Matcher[String] = AlwaysMatcher(),
              creditCard: Matcher[PaypalCreditCard] = AlwaysMatcher()): Matcher[Payer] = {
    paymentMethod ^^ { (_: Payer).getPaymentMethod aka "payment method" } and
      creditCard ^^ { (_: Payer).getFundingInstruments.get(0).getCreditCard aka "credit card" }
  }

  def beCreditCard(number: Matcher[String] = AlwaysMatcher(),
                   billingAddress: Matcher[Address] = AlwaysMatcher()): Matcher[PaypalCreditCard] = {
    number ^^ { (_: PaypalCreditCard).getNumber aka "credit card number" } and
      billingAddress ^^ { (_: PaypalCreditCard).getBillingAddress aka "billing address" }
  }


  "createAuthorize" should {
    "create a production payment with the given fields" in new Ctx {
      val someCurrencyAmount = CurrencyAmount("USD", 33.3)
      val someCreditCard = CreditCard(
        "4012888818888",
        YearMonth(2020, 12),
        Some(CreditCardOptionalFields.withFields(
          csc = Some("123"),
          holderId = Some("some holder id"),
          holderName = Some("some holder name"))))

      helper.createAuthorize(someCreditCard, someCurrencyAmount) must bePayment(
        intent = ===("authorize"),
        amount = beAmount(
          currency = ===(someCurrencyAmount.currency),
          total = ===("33.30")
        ),
        payer = bePayer(
          paymentMethod = ===("credit_card"),
          creditCard = beCreditCard(
            number = ===(someCreditCard.number),
            billingAddress = beNull[Address] // PayPal requires fields that we don't have, so we must omit completely
          )
        )
      )
    }
  }

  "retrieveAccessToken" should {
    "throw a meaningful Payment Exception (rather than a PayPal Exception)" in new Ctx {
      val someMerchant = PaypalMerchant("some client ID", "some secret")

      helper.retrieveAccessToken(someMerchant) must
        throwA[PaymentErrorException]("""PayPal Incorrect Client \(merchant\) Credentials""")
    }
  }
}
