package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.TestHouseholdState
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SocialSecurityPayrollSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  "SocialSecurity.payrollBase" should "apply contract-specific social payroll rates" in {
    val wage       = PLN(1000)
    val households = Vector(
      household(0, ContractType.Permanent, wage),
      household(1, ContractType.Zlecenie, wage),
      household(2, ContractType.B2B, wage),
      household(3, ContractType.Permanent, wage, employed = false),
    )

    val payroll = SocialSecurity.payrollBase(households)

    payroll.employed shouldBe 3
    payroll.grossWages shouldBe PLN(3000)
    val expectedZus  =
      wage * ContractType.zusEmployerRate(ContractType.Permanent) * p.social.zusScale +
        wage * ContractType.zusEmployerRate(ContractType.Zlecenie) * p.social.zusScale
    payroll.zusContributions shouldBe expectedZus
    payroll.nfzContributions shouldBe 3 * (wage * p.social.nfzContribRate)
    payroll.ppkContributions shouldBe 2 * (wage * (p.social.ppkEmployeeRate + p.social.ppkEmployerRate))
    val expectedFp   =
      wage * ContractType.fpRate(ContractType.Permanent) +
        wage * ContractType.fpRate(ContractType.Zlecenie) +
        wage * ContractType.fpRate(ContractType.B2B)
    val expectedFgsp =
      Vector(ContractType.Permanent, ContractType.Zlecenie, ContractType.B2B)
        .map:
          case ContractType.B2B => PLN.Zero
          case _                => wage * p.earmarked.fgspRate
        .foldLeft(PLN.Zero)(_ + _)
    payroll.fpContributions shouldBe expectedFp
    payroll.fgspContributions shouldBe expectedFgsp
  }

  it should "preserve aggregate formulas for legacy employed-and-wage drivers" in {
    val payroll = SocialSecurity.PayrollBase.aggregate(80000, PLN(7000))

    SocialSecurity.zusStep(payroll, 1000) shouldBe SocialSecurity.zusStep(80000, PLN(7000), 1000)
    SocialSecurity.nfzStep(payroll, 100000, 1000) shouldBe SocialSecurity.nfzStep(80000, PLN(7000), 100000, 1000)
    SocialSecurity.ppkStep(payroll) shouldBe SocialSecurity.ppkStep(80000, PLN(7000))
  }

  private def household(id: Int, contractType: ContractType, wage: PLN, employed: Boolean = true): Household.State =
    TestHouseholdState(
      id = HhId(id),
      status = if employed then HhStatus.Employed(FirmId(id), SectorIdx(0), wage) else HhStatus.Unemployed(0),
      contractType = contractType,
    )
