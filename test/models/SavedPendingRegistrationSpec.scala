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

package models

import base.SpecBase
import play.api.libs.json.{JsError, JsSuccess, Json}

import java.util.UUID

class SavedPendingRegistrationSpec extends SpecBase {

  private val userAnswers: UserAnswers = arbitraryUserAnswers.arbitrary.sample.value

  private val savedPendingRegistration: SavedPendingRegistration =
    SavedPendingRegistration(
      journeyId = userAnswers.journeyId,
      uniqueUrlCode = UUID.randomUUID().toString,
      userAnswersData = userAnswers.data,
      lastUpdated = userAnswers.lastUpdated,
      uniqueActivationCode = UUID.randomUUID().toString,
      intermediaryDetails = IntermediaryDetails("intermediaryNumber", "intermediaryName")
    )

  private val savedPendingRegistrationWithUserAnswers: SavedPendingRegistrationWithUserAnswers =
    SavedPendingRegistrationWithUserAnswers(
      journeyId = userAnswers.journeyId,
      uniqueUrlCode = UUID.randomUUID().toString,
      userAnswers = userAnswers,
      lastUpdated = userAnswers.lastUpdated,
      uniqueActivationCode = UUID.randomUUID().toString,
      intermediaryDetails = IntermediaryDetails("intermediaryNumber", "intermediaryName")
    )


  "SavedPendingRegistration" - {

    "must serialise/deserialise to and from a SavedPendingRegistration object" in {

      val json = Json.obj(
        "journeyId" -> savedPendingRegistration.journeyId,
        "uniqueUrlCode" -> savedPendingRegistration.uniqueUrlCode,
        "userAnswersData" -> savedPendingRegistration.userAnswersData,
        "lastUpdated" -> savedPendingRegistration.lastUpdated,
        "uniqueActivationCode" -> savedPendingRegistration.uniqueActivationCode,
        "intermediaryDetails" -> savedPendingRegistration.intermediaryDetails
      )

      val expectedResult: SavedPendingRegistration =
        SavedPendingRegistration(
          journeyId = savedPendingRegistration.journeyId,
          uniqueUrlCode = savedPendingRegistration.uniqueUrlCode,
          userAnswersData = savedPendingRegistration.userAnswersData,
          lastUpdated = savedPendingRegistration.lastUpdated,
          uniqueActivationCode = savedPendingRegistration.uniqueActivationCode,
          intermediaryDetails = IntermediaryDetails("intermediaryNumber", "intermediaryName")
        )

      json.validate[SavedPendingRegistration] mustBe JsSuccess(expectedResult)
      Json.toJson(expectedResult) mustBe json
    }

    "must handle missing fields during deserialization" in {

      val json = Json.obj()

      json.validate[SavedPendingRegistration] mustBe a[JsError]
    }

    "must handle invalid data during deserialisation" in {

      val json = Json.obj(
        "journeyId" -> savedPendingRegistration.journeyId,
        "uniqueCode" -> 12345,
        "userAnswersData" -> savedPendingRegistration.userAnswersData,
        "lastUpdated" -> savedPendingRegistration.lastUpdated
      )

      json.validate[SavedPendingRegistration] mustBe a[JsError]
    }
  }

  "SavedPendingRegistrationWithUserAnswers" - {

    "must serialise/deserialise to and from a SavedPendingRegistrationWithUserAnswers object" in {

      val json = Json.obj(
        "journeyId" -> savedPendingRegistrationWithUserAnswers.journeyId,
        "uniqueUrlCode" -> savedPendingRegistrationWithUserAnswers.uniqueUrlCode,
        "userAnswers" -> savedPendingRegistrationWithUserAnswers.userAnswers,
        "lastUpdated" -> savedPendingRegistrationWithUserAnswers.lastUpdated,
        "uniqueActivationCode" -> savedPendingRegistrationWithUserAnswers.uniqueActivationCode,
        "intermediaryDetails" -> savedPendingRegistrationWithUserAnswers.intermediaryDetails
      )

      val expectedResult: SavedPendingRegistrationWithUserAnswers =
        SavedPendingRegistrationWithUserAnswers(
          journeyId = savedPendingRegistrationWithUserAnswers.journeyId,
          uniqueUrlCode = savedPendingRegistrationWithUserAnswers.uniqueUrlCode,
          userAnswers = savedPendingRegistrationWithUserAnswers.userAnswers,
          lastUpdated = savedPendingRegistrationWithUserAnswers.lastUpdated,
          uniqueActivationCode = savedPendingRegistrationWithUserAnswers.uniqueActivationCode,
          intermediaryDetails = IntermediaryDetails("intermediaryNumber", "intermediaryName")
        )

      json.validate[SavedPendingRegistrationWithUserAnswers] mustBe JsSuccess(expectedResult)
      Json.toJson(expectedResult) mustBe json
    }

    "must handle missing fields during deserialization" in {

      val json = Json.obj()

      json.validate[SavedPendingRegistrationWithUserAnswers] mustBe a[JsError]
    }

    "must handle invalid data during deserialisation" in {

      val json = Json.obj(
        "journeyId" -> savedPendingRegistrationWithUserAnswers.journeyId,
        "uniqueCode" -> 12345,
        "userAnswers" -> savedPendingRegistrationWithUserAnswers.userAnswers,
        "lastUpdated" -> savedPendingRegistrationWithUserAnswers.lastUpdated
      )

      json.validate[SavedPendingRegistrationWithUserAnswers] mustBe a[JsError]
    }
  }
}
