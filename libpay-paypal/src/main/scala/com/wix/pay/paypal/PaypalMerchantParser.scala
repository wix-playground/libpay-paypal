package com.wix.pay.paypal

trait PaypalMerchantParser {
  def parse(merchantKey: String): PaypalMerchant
  def stringify(merchant: PaypalMerchant): String
}
