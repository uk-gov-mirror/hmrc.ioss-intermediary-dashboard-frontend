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

import connectors.RegistrationConnector
import logging.Logging
import models.domain.VatCustomerInfo
import models.responses.ErrorResponse
import models.{SavedPendingRegistration, SavedPendingRegistrationWithUserAnswers, UserAnswers}
import uk.gov.hmrc.domain.Vrn
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PendingRegistrationService @Inject()(
                                            registrationConnector: RegistrationConnector
                                          )(implicit ec: ExecutionContext) extends Logging {


  def getPendingRegistration(
                              intermediaryNumber: String,
                              vrn: Vrn
                            )(implicit hc: HeaderCarrier): Future[Either[ErrorResponse, Seq[SavedPendingRegistrationWithUserAnswers]]] = {

      registrationConnector.getPendingRegistrations(intermediaryNumber).flatMap {
        case Right(pendingRegistration) =>
          toSavedPendingRegistrationWithUserAnswers(pendingRegistration, vrn)
        case Left(errorResponse) =>
          val message: String = s"Unable to retrieve pending registrations for intermediary " +
            s"number [$intermediaryNumber]: $errorResponse"
          logger.error(message)
          Future.successful(Left(errorResponse))
      }
  }

  private def toSavedPendingRegistrationWithUserAnswers(
                                                         savedPendingRegistration: Seq[SavedPendingRegistration],
                                                         vrn: Vrn
                                                       )(implicit hc: HeaderCarrier): Future[Either[ErrorResponse, Seq[SavedPendingRegistrationWithUserAnswers]]] = {

    registrationConnector.getVatCustomerInfo(vrn.vrn).map {
      case Right(vatInfo) =>
        Right(savedPendingRegistration.map { savedPendingRegistration =>
          val userAnswers = UserAnswers(
            "",
            savedPendingRegistration.journeyId,
            savedPendingRegistration.userAnswersData,
            Some(vatInfo),

          )

          SavedPendingRegistrationWithUserAnswers(
            savedPendingRegistration.journeyId,
            savedPendingRegistration.uniqueUrlCode,
            userAnswers.copy(vatInfo = Some(vatInfo)),
            savedPendingRegistration.lastUpdated,
            savedPendingRegistration.uniqueActivationCode,
            savedPendingRegistration.intermediaryDetails
          )
        })
      case Left(errorResponse) =>
        logger.warn(
          s"Unable to retrieve VAT customer information for VRN " +
            s"[${vrn.vrn}]: $errorResponse"
        )
        Left(errorResponse)
    }
  }

}
