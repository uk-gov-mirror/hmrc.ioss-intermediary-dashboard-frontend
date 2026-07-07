/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package controllers.actions

import base.SpecBase
import config.FrontendAppConfig
import models.DesAddress
import models.etmp.EtmpExclusionReason.TransferringMSID
import models.etmp.{EtmpExclusion, EtmpOtherAddress}
import models.requests.RegistrationRequest
import org.scalatestplus.mockito.MockitoSugar
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import play.api.test.FakeRequest
import play.api.test.Helpers.running

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CheckNiBasedAddressFilterSpec extends SpecBase with MockitoSugar {

  class Harness(frontendAppConfig: FrontendAppConfig) extends CheckNiBasedAddressFilterImpl(frontendAppConfig) {
    def callFilter(request: RegistrationRequest[_]): Future[Option[Result]] = filter(request)
  }

  private val nonNiVatInfo = vatCustomerInfo.copy(
    desAddress = DesAddress(
      line1 = "1 The Street",
      line2 = None,
      line3 = None,
      line4 = None,
      line5 = None,
      postCode = Some("AA11 1AA"),
      countryCode = "GB"
    )
  )

  private val niVatInfo = vatCustomerInfo.copy(
    desAddress = DesAddress(
      line1 = "1 The Street",
      line2 = None,
      line3 = None,
      line4 = None,
      line5 = None,
      postCode = Some("BT11 1AA"),
      countryCode = "GB"
    )
  )

  private val niOtherAddress: EtmpOtherAddress = EtmpOtherAddress(
    issuedBy = "GB",
    tradingName = Some("Company name"),
    addressLine1 = "Other Address Line 1",
    addressLine2 = Some("Other Address Line 2"),
    townOrCity = "Other Town or City",
    regionOrState = Some("Other Region or State"),
    postcode = Some("BT11AH")
  )

  ".filter" - {

    "must return None" - {

      "when an intermediary is excluded" in {

        val excludedRegistration = registrationWrapper.copy(
          etmpDisplayRegistration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value.copy(
            exclusions = Seq(
              EtmpExclusion(
                exclusionReason = TransferringMSID,
                effectiveDate = LocalDate.of(2025, 1, 1),
                decisionDate = LocalDate.of(2025, 1, 1),
                quarantine = false
              )
            )
          )
        )

        val application = applicationBuilder(None).build()

        running(application) {

          val request = RegistrationRequest(
            FakeRequest(),
            userId = userAnswersId,
            enrolments = enrolments,
            vrn = vrn,
            intermediaryNumber = intermediaryNumber,
            registrationWrapper = excludedRegistration
          )

          val frontendAppConfig = application.injector.instanceOf[FrontendAppConfig]
          val controller = new Harness(frontendAppConfig)

          val result = controller.callFilter(request).futureValue

          result mustBe None
        }
      }

      "when VAT address is NI and otherAddress field is empty" in {

        val niVatWithEmptyOtherAddress = registrationWrapper.copy(
          vatInfo = niVatInfo,
          etmpDisplayRegistration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value.copy(
            otherAddress = None
          )
        )

        val request = RegistrationRequest(
          FakeRequest(),
          userId = userAnswersId,
          enrolments = enrolments,
          vrn = vrn,
          intermediaryNumber = intermediaryNumber,
          registrationWrapper = niVatWithEmptyOtherAddress
        )

        val application = applicationBuilder(None).build()

        running(application) {

          val frontendAppConfig = application.injector.instanceOf[FrontendAppConfig]
          val controller = new Harness(frontendAppConfig)

          val result = controller.callFilter(request).futureValue

          result mustBe None
        }
      }

      "when both VAT address and otherAddress is NI based" in {

        val registration = registrationWrapper.copy(
          vatInfo = niVatInfo,
          etmpDisplayRegistration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value.copy(
            otherAddress = Some(niOtherAddress)
          )
        )

        val application = applicationBuilder(None).build()

        running(application) {

          val request = RegistrationRequest(
            FakeRequest(),
            userId = userAnswersId,
            enrolments = enrolments,
            vrn = vrn,
            intermediaryNumber = intermediaryNumber,
            registrationWrapper = registration
          )

          val frontendAppConfig = application.injector.instanceOf[FrontendAppConfig]
          val controller = new Harness(frontendAppConfig)

          val result = controller.callFilter(request).futureValue

          result mustBe None
        }
      }

      "when VAT address is non-ni, but otherAddress is NI based" in {

        val registration = registrationWrapper.copy(
          vatInfo = nonNiVatInfo,
          etmpDisplayRegistration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value.copy(
            otherAddress = Some(niOtherAddress)
          )
        )

        val application = applicationBuilder(None).build()

        running(application) {

          val request = RegistrationRequest(
            FakeRequest(),
            userId = userAnswersId,
            enrolments = enrolments,
            vrn = vrn,
            intermediaryNumber = intermediaryNumber,
            registrationWrapper = registration
          )

          val frontendAppConfig = application.injector.instanceOf[FrontendAppConfig]
          val controller = new Harness(frontendAppConfig)

          val result = controller.callFilter(request).futureValue

          result mustBe None
        }
      }

      "when intermediary is excluded, VAT address is NI and otherAddress field is empty" in {

        val registration = registrationWrapper.copy(
          vatInfo = niVatInfo,
          etmpDisplayRegistration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value.copy(
            exclusions = Seq(
              EtmpExclusion(
                exclusionReason = TransferringMSID,
                effectiveDate = LocalDate.of(2025, 1, 1),
                decisionDate = LocalDate.of(2025, 1, 1),
                quarantine = false
              )
            ),
            otherAddress = None
          )
        )

        val application = applicationBuilder(None).build()

        running(application) {

          val request = RegistrationRequest(
            FakeRequest(),
            userId = userAnswersId,
            enrolments = enrolments,
            vrn = vrn,
            intermediaryNumber = intermediaryNumber,
            registrationWrapper = registration
          )

          val frontendAppConfig = application.injector.instanceOf[FrontendAppConfig]
          val controller = new Harness(frontendAppConfig)

          val result = controller.callFilter(request).futureValue

          result mustBe None
        }
      }

      "when intermediary is excluded, and both VAT address and otherAddress is NI based" in {

        val registration = registrationWrapper.copy(
          vatInfo = niVatInfo,
          etmpDisplayRegistration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value.copy(
            exclusions = Seq(
              EtmpExclusion(
                exclusionReason = TransferringMSID,
                effectiveDate = LocalDate.of(2025, 1, 1),
                decisionDate = LocalDate.of(2025, 1, 1),
                quarantine = false
              )
            ),
            otherAddress = Some(niOtherAddress)
          )
        )

        val application = applicationBuilder(None).build()

        running(application) {

          val request = RegistrationRequest(
            FakeRequest(),
            userId = userAnswersId,
            enrolments = enrolments,
            vrn = vrn,
            intermediaryNumber = intermediaryNumber,
            registrationWrapper = registration
          )

          val frontendAppConfig = application.injector.instanceOf[FrontendAppConfig]
          val controller = new Harness(frontendAppConfig)

          val result = controller.callFilter(request).futureValue

          result mustBe None
        }
      }
    }

    "must redirect users to the intermediary frontend service so they can provide their latest information" - {

      "when VAT address is non-NI and the otherAddress field is empty" in {

        val registration = registrationWrapper.copy(
          vatInfo = nonNiVatInfo,
          etmpDisplayRegistration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value.copy(
            exclusions = Seq.empty,
            otherAddress = None
          )
        )

        val application = applicationBuilder(None).build()

        running(application) {

          val request = RegistrationRequest(
            FakeRequest(),
            userId = userAnswersId,
            enrolments = enrolments,
            vrn = vrn,
            intermediaryNumber = intermediaryNumber,
            registrationWrapper = registration
          )

          val frontendAppConfig = application.injector.instanceOf[FrontendAppConfig]
          val controller = new Harness(frontendAppConfig)

          val result = controller.callFilter(request).futureValue

          result.value mustBe Redirect(frontendAppConfig.changeYourRegistrationUrl)
        }
      }

      "when both VAT address and otherAddress is non-NI" in {

        val registration = registrationWrapper.copy(
          vatInfo = nonNiVatInfo,
          etmpDisplayRegistration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value.copy(
            exclusions = Seq.empty,
            otherAddress = Some(
              EtmpOtherAddress(
                issuedBy = "GB",
                tradingName = Some("Company name"),
                addressLine1 = "Other Address Line 1",
                addressLine2 = Some("Other Address Line 2"),
                townOrCity = "Other Town or City",
                regionOrState = Some("Other Region or State"),
                postcode = Some("AA11AH")
              )
            )
          )
        )

        val application = applicationBuilder(None).build()

        running(application) {

          val request = RegistrationRequest(
            FakeRequest(),
            userId = userAnswersId,
            enrolments = enrolments,
            vrn = vrn,
            intermediaryNumber = intermediaryNumber,
            registrationWrapper = registration
          )

          val frontendAppConfig = application.injector.instanceOf[FrontendAppConfig]
          val controller = new Harness(frontendAppConfig)

          val result = controller.callFilter(request).futureValue

          result.value mustBe Redirect(frontendAppConfig.changeYourRegistrationUrl)
        }
      }

      "when intermediary is not excluded" in {

        val registration = registrationWrapper.copy(
          etmpDisplayRegistration = arbitraryEtmpDisplayRegistration.arbitrary.sample.value.copy(
            exclusions = Seq.empty
          )
        )

        val application = applicationBuilder(None).build()

        running(application) {

          val request = RegistrationRequest(
            FakeRequest(),
            userId = userAnswersId,
            enrolments = enrolments,
            vrn = vrn,
            intermediaryNumber = intermediaryNumber,
            registrationWrapper = registration
          )

          val frontendAppConfig = application.injector.instanceOf[FrontendAppConfig]
          val controller = new Harness(frontendAppConfig)

          val result = controller.callFilter(request).futureValue

          result.value mustBe Redirect(frontendAppConfig.changeYourRegistrationUrl)
        }
      }
    }
  }
}
