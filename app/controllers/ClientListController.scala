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
import controllers.actions.*
import logging.Logging
import models.amend.PreviousRegistration
import models.etmp.EtmpClientDetails
import pages.Waypoints
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import services.intermediaries.AccountService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewmodels.clientList.ClientListViewModel
import views.html.ClientListView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ClientListController @Inject()(
                                      override val messagesApi: MessagesApi,
                                      cc: AuthenticatedControllerComponents,
                                      frontendAppConfig: FrontendAppConfig,
                                      view: ClientListView,
                                      accountService: AccountService
                                    )(implicit ec: ExecutionContext)
  extends FrontendBaseController with I18nSupport with Logging {

  protected val controllerComponents: MessagesControllerComponents = cc

  def onPageLoad(waypoints: Waypoints): Action[AnyContent] = cc.identifyAndGetRegistration.async {
    implicit request =>

      val clientDetailsList: Seq[EtmpClientDetails] = request.registrationWrapper.etmpDisplayRegistration.clientDetails

      val numberOfIntermediaryAccounts = request.enrolments.enrolments.count(_.key == frontendAppConfig.intermediaryEnrolment)

      val previousRegistrations: Future[Seq[PreviousRegistration]] = if(numberOfIntermediaryAccounts > 1) {
        accountService.getPreviousRegistrations()
      } else {
        Future.successful(Seq.empty)
      }

      val changeRegistrationRedirectUrl: String = frontendAppConfig.changeYourNetpRegistrationUrl
      val excludeClientRedirectUrl: String = frontendAppConfig.leaveNetpServiceUrl

      val viewModel: ClientListViewModel = ClientListViewModel(
        clientDetailsList,
        changeRegistrationRedirectUrl,
        excludeClientRedirectUrl,
      )

      previousRegistrations.map { previousRegistrations =>
        val numberOfPreviousRegistrations: Int = previousRegistrations.size
        Ok(view(waypoints, viewModel, numberOfPreviousRegistrations))
      }
  }
}
