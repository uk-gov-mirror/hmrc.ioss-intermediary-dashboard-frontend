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

package controllers

import base.SpecBase
import connectors.RegistrationConnector
import models.responses.InternalServerError
import models.{IntermediaryDetails, SavedPendingRegistrationWithUserAnswers, UserAnswers}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, reset, times, verify, when}
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.mockito.MockitoSugar
import pages.{EmptyWaypoints, Waypoints}
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import services.PendingRegistrationService
import utils.FutureSyntax.FutureOps

class ClientAwaitingActivationControllerSpec extends SpecBase with MockitoSugar with BeforeAndAfterEach {

  private val waypoints: Waypoints = EmptyWaypoints

  private val mockRegistrationConnector: RegistrationConnector = mock[RegistrationConnector]
  private val mockPendingRegistrationService: PendingRegistrationService = mock[PendingRegistrationService]

  def generate6DigitCode(): String = {
    util.Random.alphanumeric.filter(_.isUpper).take(6).mkString
  }

  private def savedPendingRegistrationWithUserAnswers(userAnswers: UserAnswers): SavedPendingRegistrationWithUserAnswers =
    SavedPendingRegistrationWithUserAnswers(
      journeyId = userAnswers.journeyId,
      uniqueUrlCode = generate6DigitCode(),
      userAnswers = userAnswers,
      lastUpdated = userAnswers.lastUpdated,
      uniqueActivationCode = generate6DigitCode(),
      intermediaryDetails = IntermediaryDetails(intermediaryNumber, intermediaryName)
    )

  override def beforeEach(): Unit = reset(
    mockRegistrationConnector,
    mockPendingRegistrationService
  )

  "ClientAwaitingActivation Controller" - {

    "must return OK and the correct view for a GET" in {

      val testSavedPendingRegistrations = Seq(savedPendingRegistrationWithUserAnswers(emptyUserAnswersWithVatInfo))
      val numberOfPendingRegistrations = testSavedPendingRegistrations.size

      when(mockRegistrationConnector.getNumberOfPendingRegistrations(any())(any()))
        .thenReturn(numberOfPendingRegistrations.toLong.toFuture)

      when(mockPendingRegistrationService.getPendingRegistration(any(), any())(any()))
        .thenReturn(Right(testSavedPendingRegistrations).toFuture)

      val application = applicationBuilder(userAnswers = None)
        .overrides(
          bind[RegistrationConnector].toInstance(mockRegistrationConnector),
          bind[PendingRegistrationService].toInstance(mockPendingRegistrationService)
        )
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.ClientAwaitingActivationController.onPageLoad(waypoints).url)
        val result  = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include(testSavedPendingRegistrations.head.userAnswers.vatInfo.get.organisationName.value)

        verify(mockRegistrationConnector, times(1)).getNumberOfPendingRegistrations(any())(any())
        verify(mockPendingRegistrationService, times(1)).getPendingRegistration(any(), any())(any())
      }
    }

    "must return OK and the correct view for a GET when no pending registrations exist" in {
      val numberOfPendingRegistration = 0

      when(mockRegistrationConnector.getNumberOfPendingRegistrations(any())(any()))
        .thenReturn(numberOfPendingRegistration.toLong.toFuture)

      when(mockPendingRegistrationService.getPendingRegistration(any(), any())(any()))
        .thenReturn(Right(Seq.empty).toFuture)

      val application = applicationBuilder(userAnswers = None)
        .overrides(
          bind[RegistrationConnector].toInstance(mockRegistrationConnector),
          bind[PendingRegistrationService].toInstance(mockPendingRegistrationService)
        )
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.ClientAwaitingActivationController.onPageLoad(waypoints).url)
        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) must include("Client name")
        contentAsString(result) mustNot include("Company name")

        verify(mockRegistrationConnector, times(1)).getNumberOfPendingRegistrations(any())(any())
        verify(mockPendingRegistrationService, times(1)).getPendingRegistration(any(), any())(any())
      }
    }

    "must throw an exception when the connector fails to retrieve the number of pending registrations" in {

      when(mockRegistrationConnector.getNumberOfPendingRegistrations(any())(any()))
        .thenReturn(new RuntimeException("Failed to retrieve data.").toFuture)

      val application = applicationBuilder(userAnswers = None)
        .overrides(
          bind[RegistrationConnector].toInstance(mockRegistrationConnector),
          bind[PendingRegistrationService].toInstance(mockPendingRegistrationService)
        )
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.ClientAwaitingActivationController.onPageLoad(waypoints).url)

        assertThrows[RuntimeException] {
          route(application, request).value.futureValue
        }

        verify(mockRegistrationConnector, times(1)).getNumberOfPendingRegistrations(any())(any())
        verify(mockPendingRegistrationService, never()).getPendingRegistration(any(), any())(any())
      }
    }

    "must throw an exception and log the error when the service fails to return pending registrations" in {

      when(mockRegistrationConnector.getNumberOfPendingRegistrations(any())(any()))
        .thenReturn(0.toLong.toFuture)

      when(mockPendingRegistrationService.getPendingRegistration(any(), any())(any()))
        .thenReturn(Left(InternalServerError).toFuture)

      val application = applicationBuilder(userAnswers = None)
        .overrides(
          bind[RegistrationConnector].toInstance(mockRegistrationConnector),
          bind[PendingRegistrationService].toInstance(mockPendingRegistrationService)
        )
        .build()

      running(application) {
        val request = FakeRequest(GET, routes.ClientAwaitingActivationController.onPageLoad(waypoints).url)

        assertThrows[Exception] {
          route(application, request).value.futureValue
        }

        verify(mockRegistrationConnector, times(1)).getNumberOfPendingRegistrations(any())(any())
        verify(mockPendingRegistrationService, times(1)).getPendingRegistration(any(), any())(any())
      }
    }
  }

}
