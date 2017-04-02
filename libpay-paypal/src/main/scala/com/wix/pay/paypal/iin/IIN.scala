package com.wix.pay.paypal.iin

import java.io.File

import scala.io.Source

object IIN {
  val countryCodes: Map[String, String] = {
    val file = new File(getClass.getResource("/iin-countries.csv").toURI)

    Source.fromFile(file).getLines.map { line =>
      val index = line.indexOf(',')
      val iin = line.substring(0, index)
      val countryCode = line.substring(index + 1)

      iin -> countryCode
    }.toMap
  }
}
