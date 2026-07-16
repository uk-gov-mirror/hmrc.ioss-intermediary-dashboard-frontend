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

import config.FrontendAppConfig
import connectors.RegistrationConnector
import controllers.actions.*
import logging.Logging

import javax.inject.Inject
import pages.Waypoints
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import services.PendingRegistrationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.ClientAwaitingActivationView
import utils.FutureSyntax.FutureOps
import utils.ClientTableBuilder.buildClientsTable

import scala.concurrent.ExecutionContext

class ClientAwaitingActivationController @Inject()(
                                       override val messagesApi: MessagesApi,
                                       cc: AuthenticatedControllerComponents,
                                       val controllerComponents: MessagesControllerComponents,
                                       registrationConnector: RegistrationConnector,
                                       pendingRegistrationService: PendingRegistrationService,
                                       frontendAppConfig: FrontendAppConfig,
                                       view: ClientAwaitingActivationView
                                     )(implicit ec: ExecutionContext) extends FrontendBaseController with I18nSupport with Logging with GetClientCompanyName  {

  def onPageLoad(waypoints: Waypoints): Action[AnyContent] = (cc.actionBuilder andThen cc.identify).async {
    implicit request =>

      registrationConnector.getNumberOfPendingRegistrations(request.intermediaryNumber).map(_.toInt).flatMap { numberOfAwaitingClients =>
        pendingRegistrationService.getPendingRegistration(request.intermediaryNumber, request.vrn).flatMap {
          case Right(savedPendingRegistrations) =>

            val companyNames = savedPendingRegistrations.map{ pendingRegistration =>
              getClientCompanyName(pendingRegistration)
            }
            
            val activationExpiryDates = savedPendingRegistrations.map(_.activationExpiryDate)
            val pendingRegistrationJourneyId = savedPendingRegistrations.map { registration =>
              registration.journeyId
            }

            val clientsTable =
              buildClientsTable(companyNames, activationExpiryDates, pendingRegistrationJourneyId, frontendAppConfig)

            Ok(view(numberOfAwaitingClients, clientsTable)).toFuture

          case Left(errors) =>
            val message: String = s"Received an unexpected error when trying to retrieve a pending registration for the given intermediary number: $errors."
            val exception: Exception = new Exception(message)
            logger.error(exception.getMessage, exception)
            throw exception
        }
      }
  }
}
