package com.wix.pay.paypal


import com.paypal.api.payments.Error
import com.paypal.base.rest.PayPalRESTException
import com.wix.pay.paypal.model.ErrorNames
import com.wix.pay.{PaymentErrorException, PaymentRejectedException}
import org.specs2.mutable.SpecWithJUnit
import org.specs2.specification.Scope

class ExceptionsTranslatorTest extends SpecWithJUnit {
  private def createPaypalException(errorName: String): PayPalRESTException = {
    val e = new PayPalRESTException("some exception message")
    e.setDetails(new Error(errorName, "some error message", s"https://developer.paypal.com/webapps/developer/docs/api/#$errorName", "f31ad61f871f4"))
    e
  }

  trait Ctx extends Scope {
    val translator = new ExceptionsTranslator(
      rejectOnUnknownErrors = false
    )
    val translatorThatRejectsOnUnknownErrors = new ExceptionsTranslator(
      rejectOnUnknownErrors = true
    )
  }

  "translatePaypalException" should {
    "translate unknown errors to payment errors if rejectOnUnknownErrors is false" in new Ctx {
      translator.translatePaypalException(
        e = createPaypalException(ErrorNames.unknownError)
      ) must beAnInstanceOf[PaymentErrorException]
    }

    "translate unknown errors to payment rejection if rejectOnUnknownErrors is true" in new Ctx {
      translatorThatRejectsOnUnknownErrors.translatePaypalException(
        e = createPaypalException(ErrorNames.unknownError)
      ) must beAnInstanceOf[PaymentRejectedException]
    }

    "translate card refusal errors to payment rejection" in new Ctx {
      translator.translatePaypalException(
        e = createPaypalException(ErrorNames.creditCardRefused)
      ) must beAnInstanceOf[PaymentRejectedException]
    }

    "translate CVV check failures to payment rejection" in new Ctx {
      translator.translatePaypalException(
        e = createPaypalException(ErrorNames.creditCardCvvCheckFailed)
      ) must beAnInstanceOf[PaymentRejectedException]
    }

    "translate other errors to general payment errors" in new Ctx {
      translator.translatePaypalException(
        e = createPaypalException("some-unknown-error")
      ) must beAnInstanceOf[PaymentErrorException]
    }
  }
}
