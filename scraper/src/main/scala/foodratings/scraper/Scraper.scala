package foodratings.scraper

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

import org.slf4j.LoggerFactory

object Scraper extends App {
  override def main(args: Array[String]) {
    val log = LoggerFactory.getLogger(this.getClass)

    Workers.ResponseProcessor.start
    Workers.DataWriter.start

    log info "Started!"
    for (i <- 0 to 10) {
      val eid = 263027 + i
      log info "Sending request for " + eid
      val xmlResponse = Workers.send_request_for(eid)
      Workers.ResponseProcessor ! ResultString(eid, xmlResponse)
      Thread.sleep(200)
    }

    Workers.ResponseProcessor ! Complete
    Workers.DataWriter ! Complete
    log info "Done!"
  }
}
