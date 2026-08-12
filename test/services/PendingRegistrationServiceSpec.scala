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

package services

import base.SpecBase
import connectors.{RegistrationConnector, ReturnStatusConnector}
import models.{IntermediaryDetails, SavedPendingRegistration, SavedPendingRegistrationWithUserAnswers, StandardPeriod, UserAnswers}
import models.etmp.EtmpClientDetails
import models.responses.InternalServerError
import models.returns.SubmissionStatus.Complete
import models.returns.{CurrentReturns, Return}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, reset, times, verify, when}
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.mockito.MockitoSugar.mock
import uk.gov.hmrc.http.HeaderCarrier
import utils.FutureSyntax.FutureOps

import java.time.temporal.ChronoUnit
import java.time.{Instant, Month}
import java.util.UUID
import scala.concurrent.ExecutionContext

class PendingRegistrationServiceSpec extends SpecBase with MockitoSugar with BeforeAndAfterEach {

  implicit private lazy val hc: HeaderCarrier = HeaderCarrier()
  implicit private lazy val ec: ExecutionContext = ExecutionContext.global

  private val mockRegistrationConnector: RegistrationConnector = mock[RegistrationConnector]
  private val mockPendingRegistrationService: PendingRegistrationService = mock[PendingRegistrationService]

  private val service = new PendingRegistrationService(mockRegistrationConnector)

  private val userAnswers: UserAnswers = arbitraryUserAnswers.arbitrary.sample.value

  private val savedPendingRegistration: SavedPendingRegistration =
    SavedPendingRegistration(
      journeyId = userAnswers.journeyId,
      uniqueUrlCode = UUID.randomUUID().toString,
      userAnswersData = userAnswers.data,
      lastUpdated = userAnswers.lastUpdated,
      uniqueActivationCode = UUID.randomUUID().toString,
      intermediaryDetails = IntermediaryDetails(intermediaryNumber, intermediaryName)
    )

  override def beforeEach(): Unit = reset(
    mockRegistrationConnector,
    mockPendingRegistrationService
  )

  "getPendingRegistration" - {

    "must return enriched pending registrations when both connector calls succeed" in {

      val pendingRegistrations = Seq(savedPendingRegistration)

      when(mockRegistrationConnector.getPendingRegistrations(intermediaryNumber)).thenReturn(Right(pendingRegistrations).toFuture)

      when(mockRegistrationConnector.getVatCustomerInfo(vrn.vrn)).thenReturn(Right(vatCustomerInfo).toFuture)

      val result = service.getPendingRegistration(intermediaryNumber, vrn).futureValue

      val expectedUserAnswers =
        UserAnswers(
          "",
          savedPendingRegistration.journeyId,
          savedPendingRegistration.userAnswersData,
          Some(vatCustomerInfo)
        ).copy(lastUpdated = result.toOption.value.head.userAnswers.lastUpdated)

      val expectedRegistration =
        SavedPendingRegistrationWithUserAnswers(
          journeyId = savedPendingRegistration.journeyId,
          uniqueUrlCode = savedPendingRegistration.uniqueUrlCode,
          userAnswers = expectedUserAnswers,
          lastUpdated = savedPendingRegistration.lastUpdated,
          uniqueActivationCode = savedPendingRegistration.uniqueActivationCode,
          intermediaryDetails = savedPendingRegistration.intermediaryDetails
        )

      result mustBe Right(Seq(expectedRegistration))

      verify(mockRegistrationConnector, times(1)).getPendingRegistrations(intermediaryNumber)

      verify(mockRegistrationConnector, times(1)).getVatCustomerInfo(vrn.vrn)

    }

    "must return the error when pending registrations cannot be retrieved" in {

      when(mockRegistrationConnector.getPendingRegistrations(intermediaryNumber)).thenReturn(Left(InternalServerError).toFuture)

      val result = service.getPendingRegistration(intermediaryNumber, vrn).futureValue

      result mustBe Left(InternalServerError)

      verify(mockRegistrationConnector, times(1)).getPendingRegistrations(intermediaryNumber)

      verify(mockRegistrationConnector, never()).getVatCustomerInfo(any())(any())
    }

    "must return the error when VAT information cannot be retrieved" in {

      when(mockRegistrationConnector.getPendingRegistrations(intermediaryNumber)).thenReturn(Right(Seq(savedPendingRegistration)).toFuture)

      when(mockRegistrationConnector.getVatCustomerInfo(vrn.vrn)).thenReturn(Left(InternalServerError).toFuture)

      val result = service.getPendingRegistration(intermediaryNumber, vrn).futureValue

      result mustBe Left(InternalServerError)

      verify(mockRegistrationConnector, times(1)).getPendingRegistrations(intermediaryNumber)

      verify(mockRegistrationConnector, times(1)).getVatCustomerInfo(vrn.vrn)
    }

    "must return an empty sequence when no pending registrations exist" in {

      when(mockRegistrationConnector.getPendingRegistrations(intermediaryNumber)).thenReturn(Right(Seq.empty).toFuture)

      when(mockRegistrationConnector.getVatCustomerInfo(vrn.vrn)).thenReturn(Right(vatCustomerInfo).toFuture)

      val result = service.getPendingRegistration(intermediaryNumber, vrn).futureValue

      result mustBe Right(Seq.empty)

      verify(mockRegistrationConnector, times(1)).getVatCustomerInfo(vrn.vrn)
    }
  }
}
