package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.engine.flows.FlowMechanism
import com.boombustgroup.amorfati.engine.ledger.{AssetOwnershipContract, RuntimeMechanismSurvivability}
import com.boombustgroup.ledger.{AssetType, EntitySector, MechanismId}

/** Presentation and audit registry for ledger-derived SFC matrix evidence.
  *
  * The registry binds runtime ledger concepts to stable academic matrix rows:
  * sector ordering, instrument rows, mechanism labels, LaTeX symbols, and
  * row-level coverage policy. It is deliberately metadata-only; no matrix cell
  * value is maintained here.
  */
object SfcMatrixRegistry:

  val SchemaVersion: String = "sfc-matrix-v1"

  enum InstrumentCategory:
    case FinancialClaim
    case SettlementShell
    case UnsupportedDiagnostic

  enum RowCompleteness:
    /** The modeled holder/issuer slice is complete enough for exact row-sum
      * validation.
      */
    case Complete

    /** The row is emitted, but known holder/issuer coverage gaps are explicit
      * diagnostics rather than silent balancing rows.
      */
    case ClassifiedGap

    /** Public ledger concept is intentionally excluded from persisted matrix
      * stocks.
      */
    case Excluded

  final case class SectorMetadata(
      sector: EntitySector,
      label: String,
      shortLabel: String,
      latexSymbol: String,
      order: Int,
  )

  final case class InstrumentMetadata(
      asset: AssetType,
      label: String,
      shortLabel: String,
      latexSymbol: String,
      category: InstrumentCategory,
      completeness: RowCompleteness,
      note: String,
  )

  final case class MechanismMetadata(
      mechanism: MechanismId,
      label: String,
      shortLabel: String,
      latexSymbol: String,
      survivability: RuntimeMechanismSurvivability.Classification,
      note: String,
  )

  val sectors: Vector[SectorMetadata] =
    Vector(
      SectorMetadata(EntitySector.Households, "Households", "HH", "\\mathrm{HH}", 0),
      SectorMetadata(EntitySector.Firms, "Firms", "Firms", "\\mathrm{F}", 1),
      SectorMetadata(EntitySector.Banks, "Banks", "Banks", "\\mathrm{B}", 2),
      SectorMetadata(EntitySector.Government, "Government", "Gov", "\\mathrm{G}", 3),
      SectorMetadata(EntitySector.NBP, "NBP", "NBP", "\\mathrm{NBP}", 4),
      SectorMetadata(EntitySector.Insurance, "Insurance", "Ins", "\\mathrm{INS}", 5),
      SectorMetadata(EntitySector.Funds, "Funds", "Funds", "\\mathrm{FND}", 6),
      SectorMetadata(EntitySector.Foreign, "Foreign", "ROW", "\\mathrm{ROW}", 7),
    )

  private val sectorBySector: Map[EntitySector, SectorMetadata] =
    sectors.map(row => row.sector -> row).toMap

  private def financial(
      asset: AssetType,
      label: String,
      shortLabel: String,
      latexSymbol: String,
      completeness: RowCompleteness,
      note: String,
  ): InstrumentMetadata =
    InstrumentMetadata(asset, label, shortLabel, latexSymbol, InstrumentCategory.FinancialClaim, completeness, note)

  private def settlement(
      asset: AssetType,
      label: String,
      shortLabel: String,
      latexSymbol: String,
      note: String,
  ): InstrumentMetadata =
    InstrumentMetadata(asset, label, shortLabel, latexSymbol, InstrumentCategory.SettlementShell, RowCompleteness.Excluded, note)

  private def unsupported(
      asset: AssetType,
      label: String,
      shortLabel: String,
      latexSymbol: String,
      note: String,
  ): InstrumentMetadata =
    InstrumentMetadata(asset, label, shortLabel, latexSymbol, InstrumentCategory.UnsupportedDiagnostic, RowCompleteness.Excluded, note)

  val instruments: Vector[InstrumentMetadata] =
    Vector(
      financial(
        AssetType.DemandDeposit,
        "Demand deposits",
        "Demand dep.",
        "D",
        RowCompleteness.ClassifiedGap,
        "Household and bank demand-deposit stocks are ledger-owned; firm/fund cash is reported separately as Cash.",
      ),
      financial(
        AssetType.TermDeposit,
        "Term deposits",
        "Term dep.",
        "TD",
        RowCompleteness.ClassifiedGap,
        "Bank term-deposit liabilities are currently modeled without a holder-resolved persisted counterpart.",
      ),
      financial(
        AssetType.FirmLoan,
        "Firm loans",
        "Firm loans",
        "L_F",
        RowCompleteness.ClassifiedGap,
        "Firm loan stocks are ledger-owned on both sides, but the current dynamic-population projection can leave small holder/issuer gaps across month boundaries.",
      ),
      financial(
        AssetType.ConsumerLoan,
        "Consumer loans",
        "Consumer loans",
        "L_C",
        RowCompleteness.ClassifiedGap,
        "Consumer credit stocks are ledger-owned on both sides, but the current household/bank projection can leave small holder/issuer gaps across month boundaries.",
      ),
      financial(
        AssetType.MortgageLoan,
        "Mortgage loans",
        "Mortgages",
        "L_M",
        RowCompleteness.ClassifiedGap,
        "Household mortgage principal is persisted; the bank-side mortgage stock is not yet a supported ledger-owned row.",
      ),
      financial(
        AssetType.GovBondAFS,
        "Government bonds AFS",
        "Gov AFS",
        "B_G^{AFS}",
        RowCompleteness.Excluded,
        "Bank AFS government bonds are folded into the consolidated Government bonds row for BSM evidence.",
      ),
      financial(
        AssetType.GovBondHTM,
        "Government bonds",
        "Gov bonds",
        "B_G",
        RowCompleteness.Complete,
        "Consolidated holder/issuer government-bond row, including bank AFS and HTM books.",
      ),
      financial(
        AssetType.QuasiFiscalBond,
        "Quasi-fiscal bonds",
        "QF bonds",
        "B_{QF}",
        RowCompleteness.Complete,
        "BGK/PFR issuer and bank/NBP holder stocks are supported persisted rows.",
      ),
      financial(
        AssetType.CorpBond,
        "Corporate bonds",
        "Corp bonds",
        "B_C",
        RowCompleteness.Complete,
        "Firm issuer liabilities and supported bank, insurance, PPK, other-fund, and NBFI holdings.",
      ),
      financial(
        AssetType.Reserve,
        "Bank reserves",
        "Reserves",
        "R",
        RowCompleteness.ClassifiedGap,
        "Bank reserve assets are persisted; the NBP reserve liability is currently a runtime settlement shell.",
      ),
      settlement(
        AssetType.StandingFacility,
        "Standing facility",
        "Standing",
        "SF",
        "Standing-facility flows are runtime backstop evidence, not persisted stock evidence.",
      ),
      financial(
        AssetType.InterbankLoan,
        "Interbank loans",
        "Interbank",
        "IB",
        RowCompleteness.Complete,
        "Bank interbank positions are netted across persisted bank rows.",
      ),
      financial(
        AssetType.Equity,
        "Equity",
        "Equity",
        "EQ",
        RowCompleteness.ClassifiedGap,
        "Domestic supported holders and firm issuers are persisted; foreign ownership remains metric-only.",
      ),
      financial(
        AssetType.LifeReserve,
        "Life insurance reserves",
        "Life res.",
        "IR_L",
        RowCompleteness.ClassifiedGap,
        "Insurance-side reserve liabilities are persisted without holder-resolved household assets.",
      ),
      financial(
        AssetType.NonLifeReserve,
        "Non-life insurance reserves",
        "Non-life res.",
        "IR_N",
        RowCompleteness.ClassifiedGap,
        "Insurance-side reserve liabilities are persisted without holder-resolved household assets.",
      ),
      financial(
        AssetType.TfiUnit,
        "TFI units",
        "TFI units",
        "U_{TFI}",
        RowCompleteness.ClassifiedGap,
        "NBFI fund units are persisted as a fund-bucket liability/proxy without holder-resolved household units.",
      ),
      financial(
        AssetType.NbfiLoan,
        "NBFI loans",
        "NBFI loans",
        "L_{NBFI}",
        RowCompleteness.ClassifiedGap,
        "NBFI and quasi-fiscal loan portfolios are persisted; borrower-side coverage is incomplete.",
      ),
      financial(
        AssetType.Cash,
        "Cash and public fund balances",
        "Cash",
        "C",
        RowCompleteness.ClassifiedGap,
        "Firm cash, selected public fund cash, and NBFI cash are stock evidence; settlement cash shells are excluded.",
      ),
      unsupported(
        AssetType.Capital,
        "Bank capital",
        "Capital",
        "K_B",
        "Bank capital is persisted engine state but outside the current supported ledger-owned stock slice.",
      ),
      financial(
        AssetType.ForeignAsset,
        "Foreign assets",
        "Foreign assets",
        "FA",
        RowCompleteness.ClassifiedGap,
        "NBP foreign assets are persisted; aggregate BoP NFA remains metric-only.",
      ),
    )

  private val instrumentByAsset: Map[AssetType, InstrumentMetadata] =
    instruments.map(row => row.asset -> row).toMap

  private def mech(mechanism: MechanismId, label: String, shortLabel: String, latexSymbol: String): MechanismMetadata =
    val declaration = RuntimeMechanismSurvivability
      .declarationFor(mechanism)
      .getOrElse:
        throw new IllegalArgumentException(s"Missing survivability declaration for mechanism ${mechanism.toInt}")
    MechanismMetadata(mechanism, label, shortLabel, latexSymbol, declaration.classification, declaration.note)

  val mechanisms: Vector[MechanismMetadata] =
    Vector(
      mech(FlowMechanism.ZusContribution, "ZUS contribution", "ZUS contrib.", "ZUS_c"),
      mech(FlowMechanism.ZusPension, "ZUS pension payment", "ZUS pension", "ZUS_p"),
      mech(FlowMechanism.ZusGovSubvention, "ZUS government subvention", "ZUS subvention", "ZUS_g"),
      mech(FlowMechanism.NfzContribution, "NFZ contribution", "NFZ contrib.", "NFZ_c"),
      mech(FlowMechanism.NfzSpending, "NFZ health spending", "NFZ spend", "NFZ_s"),
      mech(FlowMechanism.NfzGovSubvention, "NFZ government subvention", "NFZ subvention", "NFZ_g"),
      mech(FlowMechanism.PpkContribution, "PPK contribution", "PPK contrib.", "PPK_c"),
      mech(FlowMechanism.PpkBondPurchase, "PPK government bond purchase", "PPK gov bond", "PPK_b"),
      mech(FlowMechanism.FpContribution, "Labour Fund contribution", "FP contrib.", "FP_c"),
      mech(FlowMechanism.FpSpending, "Labour Fund spending", "FP spend", "FP_s"),
      mech(FlowMechanism.FpGovSubvention, "Labour Fund government subvention", "FP subvention", "FP_g"),
      mech(FlowMechanism.PfronContribution, "PFRON contribution", "PFRON contrib.", "PFRON_c"),
      mech(FlowMechanism.PfronSpending, "PFRON spending", "PFRON spend", "PFRON_s"),
      mech(FlowMechanism.PfronGovSubvention, "PFRON government subvention", "PFRON subvention", "PFRON_g"),
      mech(FlowMechanism.FgspContribution, "FGSP contribution", "FGSP contrib.", "FGSP_c"),
      mech(FlowMechanism.FgspSpending, "FGSP spending", "FGSP spend", "FGSP_s"),
      mech(FlowMechanism.FgspGovSubvention, "FGSP government subvention", "FGSP subvention", "FGSP_g"),
      mech(FlowMechanism.JstRevenue, "JST revenue", "JST revenue", "JST_r"),
      mech(FlowMechanism.JstSpending, "JST spending", "JST spend", "JST_s"),
      mech(FlowMechanism.JstGovSubvention, "JST government subvention", "JST subvention", "JST_g"),
      mech(FlowMechanism.GovPurchases, "Government purchases", "Gov purchases", "G_p"),
      mech(FlowMechanism.GovDebtService, "Government debt service", "Gov debt svc.", "G_i"),
      mech(FlowMechanism.GovCapitalInvestment, "Government capital investment", "Gov capex", "G_k"),
      mech(FlowMechanism.GovUnempBenefit, "Government unemployment benefits", "Unemp benefits", "G_u"),
      mech(FlowMechanism.GovSocialTransfer, "Government social transfers", "Social transfers", "G_t"),
      mech(FlowMechanism.GovEuCofin, "Government EU co-financing", "EU cofin", "G_{eu}"),
      mech(FlowMechanism.InsLifePremium, "Life insurance premium", "Life premium", "INS_{lp}"),
      mech(FlowMechanism.InsNonLifePremium, "Non-life insurance premium", "Non-life prem.", "INS_{np}"),
      mech(FlowMechanism.InsLifeClaim, "Life insurance claim", "Life claim", "INS_{lc}"),
      mech(FlowMechanism.InsNonLifeClaim, "Non-life insurance claim", "Non-life claim", "INS_{nc}"),
      mech(FlowMechanism.InsInvestmentIncome, "Insurance investment income", "Ins income", "INS_i"),
      mech(FlowMechanism.HhConsumption, "Household consumption", "HH cons.", "HH_c"),
      mech(FlowMechanism.HhRent, "Household rent", "HH rent", "HH_r"),
      mech(FlowMechanism.HhPit, "Household PIT", "HH PIT", "HH_{pit}"),
      mech(FlowMechanism.HhDebtService, "Household debt service", "HH debt svc.", "HH_d"),
      mech(FlowMechanism.HhDepositInterest, "Household deposit interest", "HH dep. int.", "HH_i"),
      mech(FlowMechanism.HhRemittance, "Household remittance outflow", "HH remittance", "HH_{rem}"),
      mech(FlowMechanism.HhCcOrigination, "Consumer credit origination", "CC origin.", "CC_o"),
      mech(FlowMechanism.HhCcDebtService, "Consumer credit debt service", "CC service", "CC_s"),
      mech(FlowMechanism.HhCcDefault, "Consumer credit default", "CC default", "CC_d"),
      mech(FlowMechanism.HhTotalIncome, "Household total income", "HH income", "HH_y"),
      mech(FlowMechanism.FirmCit, "Firm CIT", "CIT", "F_{cit}"),
      mech(FlowMechanism.FirmLoanRepayment, "Firm loan repayment", "Firm repay", "F_{lr}"),
      mech(FlowMechanism.FirmNewLoan, "Firm new loan", "Firm loan", "F_l"),
      mech(FlowMechanism.FirmInterestPaid, "Firm interest paid", "Firm int.", "F_i"),
      mech(FlowMechanism.FirmCapex, "Firm capex", "Firm capex", "F_k"),
      mech(FlowMechanism.FirmEquityIssuance, "Firm equity issuance", "Equity iss.", "F_{eq}"),
      mech(FlowMechanism.FirmIoPayment, "Firm IO payment", "IO payment", "F_{io}"),
      mech(FlowMechanism.FirmNplDefault, "Firm NPL default", "Firm default", "F_{npl}"),
      mech(FlowMechanism.FirmProfitShifting, "Firm profit shifting", "Profit shift", "F_{ps}"),
      mech(FlowMechanism.FirmFdiRepatriation, "Firm FDI repatriation", "FDI repat.", "F_{fdi}"),
      mech(FlowMechanism.FirmGrossInvestment, "Firm gross investment", "Gross inv.", "F_{inv}"),
      mech(FlowMechanism.InvestmentDepositSettlement, "Investment deposit settlement", "Invest sett.", "INV_s"),
      mech(FlowMechanism.TfiDepositDrain, "TFI deposit drain", "TFI drain", "TFI_d"),
      mech(FlowMechanism.NbfiOrigination, "NBFI loan origination", "NBFI origin.", "NBFI_o"),
      mech(FlowMechanism.NbfiRepayment, "NBFI repayment", "NBFI repay", "NBFI_r"),
      mech(FlowMechanism.NbfiDefault, "NBFI default", "NBFI default", "NBFI_d"),
      mech(FlowMechanism.QuasiFiscalBondIssuance, "Quasi-fiscal bond issuance", "QF bond iss.", "QF_{bi}"),
      mech(FlowMechanism.QuasiFiscalBondAmortization, "Quasi-fiscal bond amortization", "QF bond amort.", "QF_{ba}"),
      mech(FlowMechanism.QuasiFiscalNbpAbsorption, "Quasi-fiscal NBP absorption", "QF NBP buy", "QF_{nbp}"),
      mech(FlowMechanism.QuasiFiscalLending, "Quasi-fiscal lending", "QF lending", "QF_l"),
      mech(FlowMechanism.QuasiFiscalRepayment, "Quasi-fiscal repayment", "QF repay", "QF_r"),
      mech(FlowMechanism.QuasiFiscalLendingDeposit, "Quasi-fiscal lending deposit", "QF lend dep.", "QF_{ld}"),
      mech(FlowMechanism.QuasiFiscalRepaymentDeposit, "Quasi-fiscal repayment deposit", "QF repay dep.", "QF_{rd}"),
      mech(FlowMechanism.QuasiFiscalNbpBondAmortization, "Quasi-fiscal NBP bond amortization", "QF NBP amort.", "QF_{nba}"),
      mech(FlowMechanism.EquityDomDividend, "Domestic equity dividend", "Dom dividend", "EQ_d"),
      mech(FlowMechanism.EquityForDividend, "Foreign equity dividend", "For dividend", "EQ_f"),
      mech(FlowMechanism.EquityDividendTax, "Equity dividend tax", "Div tax", "EQ_t"),
      mech(FlowMechanism.CorpBondCoupon, "Corporate bond coupon", "Corp coupon", "CB_c"),
      mech(FlowMechanism.CorpBondDefault, "Corporate bond default", "Corp default", "CB_d"),
      mech(FlowMechanism.CorpBondIssuance, "Corporate bond issuance", "Corp issue", "CB_i"),
      mech(FlowMechanism.CorpBondAmortization, "Corporate bond amortization", "Corp amort.", "CB_a"),
      mech(FlowMechanism.MortgageOrigination, "Mortgage origination", "Mortgage orig.", "M_o"),
      mech(FlowMechanism.MortgageRepayment, "Mortgage repayment", "Mortgage repay", "M_r"),
      mech(FlowMechanism.MortgageInterest, "Mortgage interest", "Mortgage int.", "M_i"),
      mech(FlowMechanism.MortgageDefault, "Mortgage default", "Mortgage def.", "M_d"),
      mech(FlowMechanism.TradeExports, "Trade exports", "Exports", "X"),
      mech(FlowMechanism.TradeImports, "Trade imports", "Imports", "M"),
      mech(FlowMechanism.TourismExport, "Tourism export", "Tourism X", "X_T"),
      mech(FlowMechanism.TourismImport, "Tourism import", "Tourism M", "M_T"),
      mech(FlowMechanism.Fdi, "FDI", "FDI", "FDI"),
      mech(FlowMechanism.PortfolioFlow, "Portfolio flow", "Portfolio", "PF"),
      mech(FlowMechanism.CarryTradeFlow, "Carry-trade flow", "Carry trade", "CT"),
      mech(FlowMechanism.PrimaryIncome, "Primary income", "Primary inc.", "PI"),
      mech(FlowMechanism.EuFunds, "EU funds", "EU funds", "EU"),
      mech(FlowMechanism.DiasporaInflow, "Diaspora inflow", "Diaspora", "DI"),
      mech(FlowMechanism.CapitalFlight, "Capital flight", "Cap flight", "CF"),
      mech(FlowMechanism.BankFirmInterest, "Bank firm-loan interest", "Bank firm int.", "B_{fi}"),
      mech(FlowMechanism.BankGovBondIncome, "Bank government-bond income", "Bank gov int.", "B_{gi}"),
      mech(FlowMechanism.BankNplLoss, "Bank firm NPL loss", "Bank NPL", "B_{npl}"),
      mech(FlowMechanism.BankMortgageNplLoss, "Bank mortgage NPL loss", "Bank mort. NPL", "B_{mnpl}"),
      mech(FlowMechanism.BankCcNplLoss, "Bank consumer-credit NPL loss", "Bank CC NPL", "B_{cnpl}"),
      mech(FlowMechanism.BankCorpBondLoss, "Bank corporate-bond loss", "Bank CB loss", "B_{cbl}"),
      mech(FlowMechanism.BankBfgLevy, "Bank BFG levy", "BFG levy", "B_{bfg}"),
      mech(FlowMechanism.BankUnrealizedLoss, "Bank unrealized bond loss", "Unrealized", "B_{ul}"),
      mech(FlowMechanism.BankReserveInterest, "Bank reserve interest", "Reserve int.", "B_r"),
      mech(FlowMechanism.BankStandingFacility, "Bank standing facility", "Standing fac.", "B_{sf}"),
      mech(FlowMechanism.BankInterbankInterest, "Bank interbank interest", "Interbank int.", "B_{ib}"),
      mech(FlowMechanism.BankBailIn, "Bank bail-in", "Bail-in", "B_{bail}"),
      mech(FlowMechanism.BankNbpRemittance, "Bank NBP remittance", "NBP remit.", "B_{nbp}"),
      mech(FlowMechanism.EquityGovDividend, "Government equity dividend", "Gov dividend", "EQ_g"),
      mech(FlowMechanism.NbpFxSettlement, "NBP FX settlement", "FX settle", "NBP_{fx}"),
      mech(FlowMechanism.BankStandingFacilityBackstop, "Bank standing-facility backstop", "SF backstop", "B_{sfb}"),
      mech(FlowMechanism.GovVatRevenue, "Government VAT revenue", "VAT", "G_{vat}"),
      mech(FlowMechanism.GovExciseRevenue, "Government excise revenue", "Excise", "G_{exc}"),
      mech(FlowMechanism.GovCustomsDutyRevenue, "Government customs duty revenue", "Customs", "G_{cus}"),
      mech(FlowMechanism.BankCorpBondCoupon, "Bank corporate-bond coupon", "Bank CB coupon", "B_{cbc}"),
      mech(FlowMechanism.GovBondPrimaryMarket, "Government bond primary market", "Gov bond primary", "GB_p"),
      mech(FlowMechanism.GovBondForeignPurchase, "Foreign government bond purchase", "Foreign gov bond", "GB_f"),
      mech(FlowMechanism.NbpQeGovBondPurchase, "NBP QE government bond purchase", "NBP QE", "GB_{qe}"),
      mech(FlowMechanism.InsuranceGovBondPurchase, "Insurance government bond purchase", "Ins gov bond", "GB_i"),
      mech(FlowMechanism.TfiGovBondPurchase, "TFI government bond purchase", "TFI gov bond", "GB_t"),
    )

  private val mechanismById: Map[Int, MechanismMetadata] =
    mechanisms.map(row => row.mechanism.toInt -> row).toMap

  require(
    sectorBySector.keySet == EntitySector.values.toSet,
    "SfcMatrixRegistry must classify every public EntitySector.",
  )

  require(
    instrumentByAsset.keySet == AssetType.values.toSet,
    "SfcMatrixRegistry must classify every public AssetType.",
  )

  require(
    mechanisms.map(_.mechanism.toInt).toSet == FlowMechanism.emittedRuntimeMechanisms.map(_.toInt),
    "SfcMatrixRegistry must classify every runtime-emitted FlowMechanism.",
  )

  require(
    instruments.forall(row => AssetOwnershipContract.publicAsset(row.asset).asset == row.asset),
    "SfcMatrixRegistry instruments must reference the public asset ownership contract.",
  )

  def sector(sector: EntitySector): SectorMetadata =
    sectorBySector(sector)

  def instrument(asset: AssetType): InstrumentMetadata =
    instrumentByAsset(asset)

  def mechanism(mechanism: MechanismId): MechanismMetadata =
    mechanismById(mechanism.toInt)

  def sectorOrder(sector: EntitySector): Int =
    sectorBySector(sector).order

  def instrumentOrder(asset: AssetType): Int =
    instruments.indexWhere(_.asset == asset)

  def mechanismOrder(mechanism: MechanismId): Int =
    mechanisms.indexWhere(_.mechanism == mechanism)

end SfcMatrixRegistry
