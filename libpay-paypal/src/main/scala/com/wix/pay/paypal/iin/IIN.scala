package com.wix.pay.paypal.iin

import scala.io.Source

object IIN {
  val countryCodes: Map[String, String] = {
    Source.fromInputStream(getClass.getResourceAsStream("/iin-countries.csv")).getLines.map { line =>
      val index = line.indexOf(',')
      val iin = line.substring(0, index)
      val countryCode = line.substring(index + 1)

      iin -> countryCode
    }.toMap
  }
}
