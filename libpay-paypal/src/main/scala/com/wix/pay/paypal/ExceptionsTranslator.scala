package com.wix.pay.paypal

import com.paypal.base.rest.PayPalRESTException
import com.wix.pay.paypal.model.ErrorNames
import com.wix.pay.{PaymentErrorException, PaymentException, PaymentRejectedException}

class ExceptionsTranslator(rejectOnUnknownErrors: Boolean) {
  def translatePaypalException(e: PayPalRESTException): PaymentException = {
    Option(e.getDetails) match {
      case Some(error) => error.getName match {
        case ErrorNames.creditCardRefused|ErrorNames.creditCardCvvCheckFailed => PaymentRejectedException(e.getMessage, e)
        case ErrorNames.unknownError if rejectOnUnknownErrors => PaymentRejectedException(e.getMessage, e)
        case _ => PaymentErrorException(e.getMessage, e)
      }
      case None => PaymentErrorException(e.getMessage, e)
    }
  }
}
