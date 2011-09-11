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
import org.apache.http.impl.client.DefaultHttpClient
import java.net.URI
import oauth.signpost.commonshttp.CommonsHttpOAuthConsumer
import org.apache.http.HttpRequest

object Scraper extends App {
  override def main(args: Array[String]) {
    val log = LoggerFactory.getLogger(this.getClass)
    val signer = { request: HttpRequest =>
      val consumer = new CommonsHttpOAuthConsumer(ScraperConfiguration.consumer_key, ScraperConfiguration.consumer_secret)
      consumer.sign(request)
    }

    log info "Initialising"
    Workers.ResponseProcessor.start()

    val sender = new RequestSender({() => new DefaultHttpClient()}, signer)
    val eidRegex = "\\{eid\\}".r

    log info "Started!"
    for (i <- 0 to 10) {
      val eid = 263027 + i
      log info "Sending request for " + eid

      val uri = new URI(eidRegex.replaceFirstIn(ScraperConfiguration.private_url, eid.toString))
      sender.get(uri) match {
        case Some(s: String) => Workers.ResponseProcessor ! ResultString(eid, s)
        case _ => log warn "No result for eid = " + eid
      }
    }

    sender.stop()
    Workers.ResponseProcessor ! Stop
    log info "Done!"
  }
}