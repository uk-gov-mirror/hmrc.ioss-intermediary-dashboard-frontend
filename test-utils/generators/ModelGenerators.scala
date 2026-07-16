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

package generators

import models.*
import models.amend.PreviousRegistration
import models.domain.ModelHelpers.normaliseSpaces
import models.domain.VatCustomerInfo
import models.enrolments.{EACDEnrolment, EACDEnrolments, EACDIdentifiers}
import models.etmp.*
import models.returns.{CurrentReturns, PartialReturnPeriod, Return, SubmissionStatus}
import models.saveForLater.SavedUserAnswers
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{choose, listOfN}
import org.scalatest.EitherValues
import play.api.libs.json.{JsObject, Json}

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDate, LocalDateTime, Month, ZoneOffset}
import java.util.UUID

trait ModelGenerators extends EitherValues {

  private val maxFieldLength: Int = 35

  private def commonFieldString(maxLength: Int): Gen[String] = (for {
    length <- choose(1, maxLength)
    chars <- listOfN(length, commonFieldSafeInputs)
  } yield chars.mkString).retryUntil(_.trim.nonEmpty)

  private def commonFieldSafeInputs: Gen[Char] = Gen.oneOf(
    Gen.alphaNumChar,
    Gen.oneOf('À' to 'ÿ'),
    Gen.const('.'),
    Gen.const(','),
    Gen.const('/'),
    Gen.const('’'),
    Gen.const('\''),
    Gen.const('"'),
    Gen.const('_'),
    Gen.const('&'),
    Gen.const(' '),
    Gen.const('\'')
  )


  def datesBetween(min: LocalDate, max: LocalDate): Gen[LocalDate] = {

    def toMillis(date: LocalDate): Long =
      date.atStartOfDay.atZone(ZoneOffset.UTC).toInstant.toEpochMilli

    Gen.choose(toMillis(min), toMillis(max)).map {
      millis =>
        Instant.ofEpochMilli(millis).atOffset(ZoneOffset.UTC).toLocalDate
    }
  }

  implicit lazy val arbitraryDate: Arbitrary[LocalDate] = {
    Arbitrary {
      datesBetween(LocalDate.of(2021, 7, 1), LocalDate.of(2023, 12, 31))
    }
  }

  implicit lazy val arbitraryDesAddress: Arbitrary[DesAddress] =
    Arbitrary {
      for {
        line1 <- commonFieldString(maxFieldLength)
        line2 <- Gen.option(commonFieldString(maxFieldLength))
        line3 <- Gen.option(commonFieldString(maxFieldLength))
        line4 <- Gen.option(commonFieldString(maxFieldLength))
        line5 <- Gen.option(commonFieldString(maxFieldLength))
        postCode <- Gen.option(arbitrary[String])
        country <- Gen.oneOf(Country.internationalCountries.map(_.code))
      } yield DesAddress(
        normaliseSpaces(line1),
        normaliseSpaces(line2),
        normaliseSpaces(line3),
        normaliseSpaces(line4),
        normaliseSpaces(line5),
        normaliseSpaces(postCode),
        country
      )
    }

  implicit val arbitraryVatCustomerInfo: Arbitrary[VatCustomerInfo] = {
    Arbitrary {
      for {
        desAddress <- arbitraryDesAddress.arbitrary
        registrationDate <- arbitraryDate.arbitrary
        organisationName <- Gen.alphaStr
        individualName <- Gen.alphaStr
        singleMarketIndicator <- arbitrary[Boolean]
      } yield {
        VatCustomerInfo(
          desAddress = desAddress,
          registrationDate = registrationDate,
          organisationName = Some(organisationName),
          individualName = Some(individualName),
          singleMarketIndicator = singleMarketIndicator,
          deregistrationDecisionDate = None
        )
      }
    }
  }

  implicit lazy val arbitraryUserAnswers: Arbitrary[UserAnswers] = {
    Arbitrary {
      for {
        id <- arbitrary[String]
        journeyId = UUID.randomUUID().toString
        data = JsObject(Seq("test" -> Json.toJson("test")))
        vatInfo <- arbitraryVatCustomerInfo.arbitrary
        lastUpdated = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      } yield {
        UserAnswers(
          id = id,
          journeyId = journeyId,
          data = data,
          vatInfo = Some(vatInfo),
          lastUpdated = lastUpdated
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpClientDetails: Arbitrary[EtmpClientDetails] = {
    Arbitrary {
      for {
        clientName <- Gen.alphaStr
        clientIossID <- arbitraryIossNumber.arbitrary
        clientExcluded <- arbitrary[Boolean]
      } yield {
        EtmpClientDetails(
          clientName = clientName,
          clientIossID = clientIossID,
          clientExcluded = clientExcluded
        )
      }
    }
  }

  implicit lazy val arbitrarySavedPendingRegistration: Arbitrary[SavedPendingRegistration] = {
    Arbitrary {
      for {
        userAnswers <- arbitraryUserAnswers.arbitrary
        uniqueUrlCode = UUID.randomUUID().toString
        uniqueActivationCode = UUID.randomUUID().toString
      } yield {
        SavedPendingRegistration(
          journeyId = userAnswers.journeyId,
          uniqueUrlCode = uniqueUrlCode,
          userAnswersData = userAnswers.data,
          lastUpdated = userAnswers.lastUpdated,
          uniqueActivationCode = uniqueActivationCode,
          intermediaryDetails = IntermediaryDetails("IM123456789", "IntermediaryName"))
      }
    }
  }

  implicit lazy val arbitraryBic: Arbitrary[Bic] = {
    val asciiCodeForA = 65
    val asciiCodeForN = 78
    val asciiCodeForP = 80
    val asciiCodeForZ = 90

    Arbitrary {
      for {
        firstChars <- Gen.listOfN(6, Gen.alphaUpperChar).map(_.mkString)
        char7 <- Gen.oneOf(Gen.alphaUpperChar, Gen.choose(2, 9).map(_.toString.head))
        char8 <- Gen.oneOf(
          Gen.choose(asciiCodeForA, asciiCodeForN).map(_.toChar),
          Gen.choose(asciiCodeForP, asciiCodeForZ).map(_.toChar),
          Gen.choose(0, 9).map(_.toString.head)
        )
        lastChars <- Gen.option(Gen.listOfN(3, Gen.oneOf(Gen.alphaUpperChar, Gen.numChar)).map(_.mkString))
      } yield Bic(s"$firstChars$char7$char8${lastChars.getOrElse("")}").get
    }
  }

  implicit lazy val arbitraryIban: Arbitrary[Iban] = {
    Arbitrary {
      Gen.oneOf(
        "GB94BARC10201530093459",
        "GB33BUKB20201555555555",
        "DE29100100100987654321",
        "GB24BKEN10000031510604",
        "GB27BOFI90212729823529",
        "GB17BOFS80055100813796",
        "GB92BARC20005275849855",
        "GB66CITI18500812098709",
        "GB15CLYD82663220400952",
        "GB26MIDL40051512345674",
        "GB76LOYD30949301273801",
        "GB25NWBK60080600724890",
        "GB60NAIA07011610909132",
        "GB29RBOS83040210126939",
        "GB79ABBY09012603367219",
        "GB21SCBL60910417068859",
        "GB42CPBK08005470328725"
      ).map(v => Iban(v).toOption.get)
    }
  }

  implicit lazy val arbitraryEtmpAdminUse: Arbitrary[EtmpAdminUse] = {
    Arbitrary {
      for {
        changeDate <- arbitrary[LocalDateTime]
      } yield EtmpAdminUse(changeDate = Some(changeDate))
    }
  }

  implicit lazy val arbitraryEtmpBankDetails: Arbitrary[EtmpBankDetails] = {
    Arbitrary {
      for {
        accountName <- arbitrary[String]
        bic <- arbitraryBic.arbitrary
        iban <- arbitraryIban.arbitrary
      } yield {
        EtmpBankDetails(
          accountName = accountName,
          bic = Some(bic),
          iban = iban
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpCustomerIdentification: Arbitrary[EtmpCustomerIdentification] = {
    Arbitrary {
      for {
        etmpIdType <- Gen.oneOf(EtmpIdType.values)
        vrn <- Gen.alphaStr
      } yield EtmpCustomerIdentification(etmpIdType, vrn)
    }
  }

  implicit lazy val arbitraryCountry: Arbitrary[Country] = {
    Arbitrary {
      Gen.oneOf(Country.euCountries)
    }
  }

  implicit lazy val genEuTaxReference: Gen[String] = {
    Gen.listOfN(20, Gen.alphaNumChar).map(_.mkString)
  }

  implicit lazy val arbitraryEuVatNumber: Gen[String] = {
    for {
      vatNumber <- Gen.alphaNumStr
    } yield vatNumber
  }

  implicit lazy val arbitraryEtmpTradingName: Arbitrary[EtmpTradingName] = {
    Arbitrary {
      for {
        tradingName <- Gen.alphaStr
      } yield EtmpTradingName(tradingName)
    }
  }

  implicit lazy val arbitraryEtmpDisplayEuRegistrationDetails: Arbitrary[EtmpDisplayEuRegistrationDetails] = {
    Arbitrary {
      for {
        issuedBy <- arbitraryCountry.arbitrary.map(_.code)
        vatNumber <- arbitraryEuVatNumber
        taxIdentificationNumber <- genEuTaxReference
        fixedEstablishmentTradingName <- arbitraryEtmpTradingName.arbitrary.map(_.tradingName)
        fixedEstablishmentAddressLine1 <- Gen.alphaStr
        fixedEstablishmentAddressLine2 <- Gen.alphaStr
        townOrCity <- Gen.alphaStr
        regionOrState <- Gen.alphaStr
        postcode <- Gen.alphaStr
      } yield {
        EtmpDisplayEuRegistrationDetails(
          issuedBy = issuedBy,
          vatNumber = Some(vatNumber),
          taxIdentificationNumber = Some(taxIdentificationNumber),
          fixedEstablishmentTradingName = fixedEstablishmentTradingName,
          fixedEstablishmentAddressLine1 = fixedEstablishmentAddressLine1,
          fixedEstablishmentAddressLine2 = Some(fixedEstablishmentAddressLine2),
          townOrCity = townOrCity,
          regionOrState = Some(regionOrState),
          postcode = Some(postcode)
        )
      }
    }
  }

  implicit lazy val genIntermediaryNumber: Gen[String] = {
    for {
      intermediaryNumber <- Gen.listOfN(12, Gen.alphaChar).map(_.mkString)
    } yield intermediaryNumber
  }

  implicit lazy val arbitraryOtherIossIntermediaryRegistrations: Arbitrary[EtmpOtherIossIntermediaryRegistrations] = {
    Arbitrary {
      for {
        issuedBy <- arbitraryCountry.arbitrary.map(_.code)
        intermediaryNumber <- genIntermediaryNumber
      } yield {
        EtmpOtherIossIntermediaryRegistrations(
          issuedBy = issuedBy,
          intermediaryNumber = intermediaryNumber
        )
      }
    }
  }

  implicit lazy val arbitraryIntermediaryDetails: Arbitrary[EtmpIntermediaryDetails] = {
    Arbitrary {
      for {
        otherIossIntermediaryRegistrations <- Gen.listOfN(2, arbitraryOtherIossIntermediaryRegistrations.arbitrary)
      } yield {
        EtmpIntermediaryDetails(
          otherIossIntermediaryRegistrations = otherIossIntermediaryRegistrations
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpOtherAddress: Arbitrary[EtmpOtherAddress] = {
    Arbitrary {
      for {
        issuedBy <- Gen.listOfN(2, Gen.alphaChar).map(_.mkString)
        tradingName <- Gen.listOfN(20, Gen.alphaChar).map(_.mkString)
        addressLine1 <- Gen.listOfN(35, Gen.alphaChar).map(_.mkString)
        addressLine2 <- Gen.listOfN(35, Gen.alphaChar).map(_.mkString)
        townOrCity <- Gen.listOfN(35, Gen.alphaChar).map(_.mkString)
        regionOrState <- Gen.listOfN(35, Gen.alphaChar).map(_.mkString)
        postcode <- Gen.listOfN(35, Gen.alphaChar).map(_.mkString)
      } yield EtmpOtherAddress(
        issuedBy,
        Some(tradingName),
        addressLine1,
        Some(addressLine2),
        townOrCity,
        Some(regionOrState),
        Some(postcode)
      )
    }
  }

  implicit lazy val arbitraryEtmpDisplaySchemeDetails: Arbitrary[EtmpDisplaySchemeDetails] = {
    Arbitrary {
      for {
        commencementDate <- arbitrary[LocalDate].map(_.toString)
        euRegistrationDetails <- Gen.listOfN(3, arbitraryEtmpDisplayEuRegistrationDetails.arbitrary)
        contactName <- Gen.alphaStr
        businessTelephoneNumber <- Gen.alphaNumStr
        businessEmailId <- Gen.alphaStr
        unusableStatus <- arbitrary[Boolean]
        nonCompliant <- Gen.oneOf("1", "2")
      } yield {
        EtmpDisplaySchemeDetails(
          commencementDate = commencementDate,
          euRegistrationDetails = euRegistrationDetails,
          contactName = contactName,
          businessTelephoneNumber = businessTelephoneNumber,
          businessEmailId = businessEmailId,
          unusableStatus = unusableStatus,
          nonCompliantReturns = Some(nonCompliant),
          nonCompliantPayments = Some(nonCompliant)
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpExclusion: Arbitrary[EtmpExclusion] = {
    Arbitrary {
      for {
        exclusionReason <- Gen.oneOf(EtmpExclusionReason.values)
        effectiveDate <- arbitrary[LocalDate]
        decisionDate <- arbitrary[LocalDate]
        quarantine <- arbitrary[Boolean]
      } yield {
        EtmpExclusion(
          exclusionReason = exclusionReason,
          effectiveDate = effectiveDate,
          decisionDate = decisionDate,
          quarantine = quarantine
        )
      }
    }
  }

  implicit lazy val arbitraryEtmpDisplayRegistration: Arbitrary[EtmpDisplayRegistration] = {
    Arbitrary {
      for {
        customerIdentification <- arbitraryEtmpCustomerIdentification.arbitrary
        tradingNames <- Gen.listOfN(3, arbitraryEtmpTradingName.arbitrary)
        clientDetails <- Gen.listOfN(3, arbitraryEtmpClientDetails.arbitrary)
        intermediaryDetails <- arbitraryIntermediaryDetails.arbitrary
        otherAddress <- arbitraryEtmpOtherAddress.arbitrary
        schemeDetails <- arbitraryEtmpDisplaySchemeDetails.arbitrary
        exclusions <- Gen.listOfN(1, arbitraryEtmpExclusion.arbitrary)
        bankDetails <- arbitraryEtmpBankDetails.arbitrary
        adminUse <- arbitraryEtmpAdminUse.arbitrary
      } yield {
        EtmpDisplayRegistration(
          customerIdentification = customerIdentification,
          tradingNames = tradingNames,
          clientDetails = clientDetails,
          intermediaryDetails = Some(intermediaryDetails),
          otherAddress = Some(otherAddress),
          schemeDetails = schemeDetails,
          exclusions = exclusions,
          bankDetails = bankDetails,
          adminUse = adminUse
        )
      }
    }
  }

  implicit lazy val arbitraryRegistrationWrapper: Arbitrary[RegistrationWrapper] = {
    Arbitrary {
      for {
        vatInfo <- arbitraryVatCustomerInfo.arbitrary
        etmpDisplayRegistration <- arbitraryEtmpDisplayRegistration.arbitrary
      } yield {
        RegistrationWrapper(
          vatInfo = vatInfo,
          etmpDisplayRegistration = etmpDisplayRegistration
        )
      }
    }
  }

  implicit lazy val arbitraryEACDIdentifiers: Arbitrary[EACDIdentifiers] = {
    Arbitrary {
      for {
        key <- Gen.alphaStr
        value <- Gen.alphaStr
      } yield EACDIdentifiers(
        key = key,
        value = value
      )
    }
  }

  implicit lazy val arbitraryEACDEnrolment: Arbitrary[EACDEnrolment] = {
    Arbitrary {
      for {
        service <- Gen.alphaStr
        state <- Gen.alphaStr
        identifiers <- Gen.listOfN(3, arbitraryEACDIdentifiers.arbitrary)
      } yield EACDEnrolment(
        service = service,
        state = state,
        activationDate = Some(LocalDateTime.now().truncatedTo(ChronoUnit.MILLIS)),
        identifiers = identifiers
      )
    }
  }

  implicit lazy val arbitraryEACDEnrolments: Arbitrary[EACDEnrolments] = {
    Arbitrary {
      for {
        enrolments <- Gen.listOfN(3, arbitraryEACDEnrolment.arbitrary)
      } yield EACDEnrolments(
        enrolments = enrolments
      )
    }
  }

  implicit lazy val arbitraryPreviousRegistration: Arbitrary[PreviousRegistration] = {
    Arbitrary {
      for {
        intermediaryNumber <- genIntermediaryNumber
        startPeriod <- Gen.choose(LocalDate.of(2020, 1, 1).toEpochDay, LocalDate.of(2025, 12, 31).toEpochDay)
          .map(LocalDate.ofEpochDay)
        endPeriod <- Gen.choose(startPeriod.toEpochDay, startPeriod.plusMonths(12).toEpochDay)
          .map(LocalDate.ofEpochDay)
      } yield PreviousRegistration(
        intermediaryNumber = intermediaryNumber,
        startPeriod = startPeriod,
        endPeriod = endPeriod
      )
    }
  }

  implicit lazy val arbitraryPeriod: Arbitrary[Period] = {
    Arbitrary {
      for {
        year <- Gen.choose(2022, 2099)
        month <- Gen.oneOf(Month.values.toSeq)
      } yield StandardPeriod(
        year = year,
        month = month
      )
    }
  }

  implicit lazy val arbitraryReturn: Arbitrary[Return] = {
    Arbitrary {
      for {
        period <- arbitraryPeriod.arbitrary
        submissionStatus <- Gen.oneOf(SubmissionStatus.values)
      } yield {
        Return(
          period = period,
          firstDay = period.firstDay,
          lastDay = period.lastDay,
          dueDate = period.paymentDeadline,
          submissionStatus = submissionStatus,
          inProgress = false,
          isOldest = false
        )
      }
    }
  }

  implicit lazy val arbitraryIossNumber: Arbitrary[String] = {
    Arbitrary {
      for {
        iossNumber <- Gen.listOfN(7, Gen.numChar).map(_.mkString)
      } yield {
        s"IM900$iossNumber"
      }
    }
  }

  implicit lazy val arbitraryCurrentReturns: Arbitrary[CurrentReturns] = {
    Arbitrary {
      for {
        iossNumber <- arbitraryIossNumber.arbitrary
        incompleteReturns <- Gen.listOfN(2, arbitraryReturn.arbitrary)
        completedReturns <- Gen.listOfN(2, arbitraryReturn.arbitrary)
      } yield {
        CurrentReturns(
          iossNumber = iossNumber,
          incompleteReturns = incompleteReturns,
          completedReturns = completedReturns,
          finalReturnsCompleted = false
        )
      }
    }
  }

  implicit lazy val arbitraryPartialReturnPeriod: Arbitrary[PartialReturnPeriod] = {
    Arbitrary {
      for {
        firstDay <- arbitraryDate.arbitrary
        lastDay <- arbitraryDate.arbitrary
        year <- arbitraryDate.arbitrary.map(_.getYear)
        month <- arbitraryDate.arbitrary.map(_.getMonth)
      } yield {
        PartialReturnPeriod(
          firstDay = firstDay,
          lastDay = lastDay,
          year = year,
          month = month
        )
      }
    }
  }

  implicit val arbitrarySavedUserAnswers: Arbitrary[SavedUserAnswers] = {
    Arbitrary {
      for {
        iossNumber <- arbitrary[String]
        period <- arbitraryPeriod.arbitrary
        data = JsObject(Seq("test" -> Json.toJson("test")))
        now = Instant.now
      } yield SavedUserAnswers(iossNumber, period, data, now)
    }
  }
}
