package com.wix.pay.paypal

import java.text.DecimalFormat

object PaypalHelper {
  private val amountFormat = new DecimalFormat("0.00")

  def toPaypalAmount(amount: Double): String = {
    amountFormat.format(amount)
  }
}
