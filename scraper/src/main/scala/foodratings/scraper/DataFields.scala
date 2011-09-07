package foodratings.scraper

import collection.immutable.HashMap

/*
 * Copyright (c) 2011 by Jonathan Abourbih.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

object IDsToAttributes {
  val attributes = Map(
    "ctl00_ContentPlaceHolder1_uxBusinessName"-> "name",
    "ctl00_ContentPlaceHolder1_uxBusinessAddress1"-> "address1",
    "ctl00_ContentPlaceHolder1_uxBusinessAddress2"-> "address2",
    "ctl00_ContentPlaceHolder1_uxBusinessAddress3"-> "address3",
    "ctl00_ContentPlaceHolder1_uxBusinessAddress4"-> "address4",
    "ctl00_ContentPlaceHolder1_uxBusinessPostCode"-> "postcode",
    "ctl00_ContentPlaceHolder1_uxBusinessType"-> "type",
    "ctl00_ContentPlaceHolder1_uxBusinessLastInspection"-> "last_inspection",
    "ctl00_ContentPlaceHolder1_uxLocalAuthorityName"-> "local_auth_name",
    "ctl00_ContentPlaceHolder1_uxLocalAuthorityEmail"-> "local_auth_email",
    "ctl00_ContentPlaceHolder1_uxLocalAuthorityUrl"-> "local_auth_url"
  )

  val RATING = "rating"
  val RATING_SYSTEM = "rating_system"

  val RATING_SYSTEM_FHIS = "fhis"
  val RATING_SYSTEM_FHR = "fhr"

  val fhisRatings = Map(
    "ctl00_ContentPlaceHolder1_infoImageImprovementRequired" -> "improvement_required",
    "ctl00_ContentPlaceHolder1_infoImagePassAndEatSafe" -> "pass_plus_eat_safe",
    "ctl00_ContentPlaceHolder1_infoImagePass" -> "pass",
    "ctl00_ContentPlaceHolder1_infoImageAwaitingInspection" -> "awaiting_inspection",
    "ctl00_ContentPlaceHolder1_infoImageExempt" -> "exempt"
  )
}
