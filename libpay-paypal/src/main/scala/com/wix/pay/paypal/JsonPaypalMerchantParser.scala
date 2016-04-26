package com.wix.pay.paypal

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

class JsonPaypalMerchantParser() extends PaypalMerchantParser {
  implicit val formats = DefaultFormats

  override def parse(merchantKey: String): PaypalMerchant = {
    Serialization.read[PaypalMerchant](merchantKey)
  }

  override def stringify(merchant: PaypalMerchant): String = {
    Serialization.write(merchant)
  }
}
