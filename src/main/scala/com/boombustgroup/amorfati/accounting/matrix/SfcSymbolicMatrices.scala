package com.boombustgroup.amorfati.accounting.matrix

import com.boombustgroup.amorfati.engine.flows.FlowMechanism
import com.boombustgroup.ledger.{AssetType, EntitySector, MechanismId}

object SfcSymbolicMatrices:

  final case class SymbolicRow(
      rowKey: String,
      label: String,
      cells: Map[EntitySector, String],
      zeroSymbol: String,
      assets: Vector[AssetType],
      mechanisms: Vector[MechanismId],
      note: String,
  )

  final case class SymbolicMatrix(
      name: String,
      title: String,
      rowHeader: String,
      rows: Vector[SymbolicRow],
  )

  final case class MappingRow(
      matrix: String,
      rowKey: String,
      rowLabel: String,
      symbols: Vector[String],
      assets: Vector[AssetType],
      mechanisms: Vector[MechanismId],
      note: String,
  )

  val sectors: Vector[EntitySector] =
    SfcMatrixRegistry.sectors.map(_.sector)

  private val HH    = EntitySector.Households
  private val Firms = EntitySector.Firms
  private val Banks = EntitySector.Banks
  private val Gov   = EntitySector.Government
  private val NBP   = EntitySector.NBP
  private val Ins   = EntitySector.Insurance
  private val Funds = EntitySector.Funds
  private val ROW   = EntitySector.Foreign

  private def row(
      rowKey: String,
      label: String,
      cells: (EntitySector, String)*,
  )(
      zeroSymbol: String = "0",
      assets: Vector[AssetType] = Vector.empty,
      mechanisms: Vector[MechanismId] = Vector.empty,
      note: String = "",
  ): SymbolicRow =
    SymbolicRow(rowKey, label, cells.toMap, zeroSymbol, assets, mechanisms, note)

  val balanceSheet: SymbolicMatrix =
    SymbolicMatrix(
      name = "symbolic-bsm",
      title = "Symbolic Balance Sheet Matrix",
      rowHeader = "Instrument \\ Sector",
      rows = Vector(
        row("cash", "Cash and public balances", HH -> "+H_h", Firms -> "+H_f", Banks -> "+H_b", NBP -> "-H", Funds -> "+H_{pub}")(
          assets = Vector(AssetType.Cash),
          note = "Cash covers persisted firm, selected public-fund, and NBFI cash plus the paper-level currency shell.",
        ),
        row("demand-deposits", "Demand deposits", HH -> "+D_h", Firms -> "+D_f", Banks -> "-D", Funds -> "+D_{fnd}")(
          assets = Vector(AssetType.DemandDeposit),
        ),
        row("term-deposits", "Term deposits", HH -> "+TD_h", Banks -> "-TD")(
          assets = Vector(AssetType.TermDeposit),
        ),
        row("loans", "Loans", HH -> "-L_h", Firms -> "-L_f", Banks -> "+L")(
          assets = Vector(AssetType.FirmLoan, AssetType.ConsumerLoan, AssetType.MortgageLoan, AssetType.NbfiLoan),
          note = "Loan rows aggregate firm, consumer, mortgage, and NBFI credit instruments.",
        ),
        row("reserves", "Bank reserves", Banks -> "+R", NBP -> "-R")(
          assets = Vector(AssetType.Reserve),
          note = "The NBP reserve-liability side is a runtime settlement shell in the current engine.",
        ),
        row(
          "government-bonds",
          "Government bonds",
          Banks -> "+B_b",
          Gov   -> "-B_g",
          NBP   -> "+B_{nbp}",
          Ins   -> "+B_{ins}",
          Funds -> "+B_{fnd}",
          ROW   -> "+B_{row}",
        )(
          assets = Vector(AssetType.GovBondHTM, AssetType.GovBondAFS),
        ),
        row("quasi-fiscal-bonds", "Quasi-fiscal bonds", Banks -> "+Q_b", NBP -> "+Q_{nbp}", Funds -> "-Q")(
          assets = Vector(AssetType.QuasiFiscalBond),
        ),
        row("corporate-bonds", "Corporate bonds", Firms -> "-B_c", Banks -> "+B_{cb}", Ins -> "+B_{ci}", Funds -> "+B_{cf}")(
          assets = Vector(AssetType.CorpBond),
        ),
        row("equity", "Equity", HH -> "+E_h", Firms -> "-E", Ins -> "+E_i", Funds -> "+E_f", ROW -> "+E_{row}")(
          assets = Vector(AssetType.Equity),
          note = "Foreign equity ownership is currently metric-level evidence rather than a holder-resolved persisted stock.",
        ),
        row("insurance-reserves", "Insurance reserves", HH -> "+IR_h", Ins -> "-IR")(
          assets = Vector(AssetType.LifeReserve, AssetType.NonLifeReserve),
        ),
        row("fund-units", "Fund units", HH -> "+U_h", Funds -> "-U")(
          assets = Vector(AssetType.TfiUnit),
        ),
        row("foreign-assets", "Foreign assets", NBP -> "+FA", ROW -> "-FA")(
          assets = Vector(AssetType.ForeignAsset),
        ),
        row(
          "net-worth",
          "Net worth",
          HH    -> "-NW_h",
          Firms -> "-NW_f",
          Banks -> "-NW_b",
          Gov   -> "-NW_g",
          NBP   -> "-NW_{nbp}",
          Ins   -> "-NW_{ins}",
          Funds -> "-NW_{fnd}",
          ROW   -> "-NW_{row}",
        )(
          note = "Column-balancing row used for the paper-level presentation; it is not emitted as a runtime asset.",
        ),
      ),
    )

  val transactionsFlow: SymbolicMatrix =
    SymbolicMatrix(
      name = "symbolic-tfm",
      title = "Symbolic Transactions Flow Matrix",
      rowHeader = "Flow \\ Sector",
      rows = Vector(
        row("consumption", "Consumption", HH -> "-C_h", Firms -> "+C_h")(
          mechanisms = Vector(FlowMechanism.HhConsumption),
        ),
        row("wages-and-income", "Wages and household income", HH -> "+W", Firms -> "-W")(
          mechanisms = Vector(FlowMechanism.HhTotalIncome),
        ),
        row("taxes", "Taxes", HH -> "-T_h", Firms -> "-T_f", Banks -> "-T_b", Gov -> "+T")(
          mechanisms = Vector(
            FlowMechanism.HhPit,
            FlowMechanism.FirmCit,
            FlowMechanism.GovVatRevenue,
            FlowMechanism.GovExciseRevenue,
            FlowMechanism.GovCustomsDutyRevenue,
            FlowMechanism.EquityDividendTax,
          ),
        ),
        row("social-contributions", "Social contributions", HH -> "-SC_h", Firms -> "-SC_f", Funds -> "+SC")(
          mechanisms = Vector(
            FlowMechanism.ZusContribution,
            FlowMechanism.NfzContribution,
            FlowMechanism.PpkContribution,
            FlowMechanism.FpContribution,
            FlowMechanism.PfronContribution,
            FlowMechanism.FgspContribution,
          ),
        ),
        row("transfers", "Transfers and benefits", HH -> "+TR_h", Gov -> "-TR_g", Funds -> "-TR_f")(
          mechanisms = Vector(
            FlowMechanism.ZusPension,
            FlowMechanism.GovUnempBenefit,
            FlowMechanism.GovSocialTransfer,
          ),
        ),
        row("government-spending", "Government spending", Firms -> "+G", Gov -> "-G")(
          mechanisms = Vector(
            FlowMechanism.GovPurchases,
            FlowMechanism.GovCapitalInvestment,
            FlowMechanism.NfzSpending,
            FlowMechanism.FpSpending,
            FlowMechanism.PfronSpending,
            FlowMechanism.FgspSpending,
            FlowMechanism.JstSpending,
          ),
        ),
        row("insurance", "Insurance premiums and claims", HH -> "-P+CL", Ins -> "+P-CL")(
          mechanisms = Vector(
            FlowMechanism.InsLifePremium,
            FlowMechanism.InsNonLifePremium,
            FlowMechanism.InsLifeClaim,
            FlowMechanism.InsNonLifeClaim,
            FlowMechanism.InsInvestmentIncome,
          ),
        ),
        row("loan-interest", "Loan interest", HH -> "-rL_h", Firms -> "-rL_f", Banks -> "+rL")(
          mechanisms = Vector(
            FlowMechanism.HhDebtService,
            FlowMechanism.FirmInterestPaid,
            FlowMechanism.MortgageInterest,
            FlowMechanism.BankFirmInterest,
          ),
        ),
        row("deposit-and-reserve-interest", "Deposit and reserve interest", HH -> "+rD_h", Banks -> "-rD+rR", NBP -> "-rR")(
          mechanisms = Vector(
            FlowMechanism.HhDepositInterest,
            FlowMechanism.BankReserveInterest,
            FlowMechanism.BankStandingFacility,
            FlowMechanism.BankInterbankInterest,
          ),
        ),
        row("bond-coupons", "Bond coupons", Firms -> "-iB_c", Banks -> "+iB_b", Gov -> "-iB_g", Ins -> "+iB_i", Funds -> "+iB_f", ROW -> "+iB_{row}")(
          mechanisms = Vector(
            FlowMechanism.GovDebtService,
            FlowMechanism.BankGovBondIncome,
            FlowMechanism.CorpBondCoupon,
            FlowMechanism.BankCorpBondCoupon,
          ),
        ),
        row("dividends", "Dividends", HH -> "+Div_h", Firms -> "-Div", Gov -> "+Div_g", Ins -> "+Div_i", Funds -> "+Div_f", ROW -> "+Div_{row}")(
          mechanisms = Vector(
            FlowMechanism.EquityDomDividend,
            FlowMechanism.EquityForDividend,
            FlowMechanism.EquityGovDividend,
          ),
        ),
        row("external", "External trade and income", HH -> "+REM", Firms -> "+X-M", Gov -> "+EU", ROW -> "-NX-REM-EU")(
          mechanisms = Vector(
            FlowMechanism.TradeExports,
            FlowMechanism.TradeImports,
            FlowMechanism.TourismExport,
            FlowMechanism.TourismImport,
            FlowMechanism.PrimaryIncome,
            FlowMechanism.EuFunds,
            FlowMechanism.DiasporaInflow,
            FlowMechanism.HhRemittance,
          ),
        ),
        row("loan-origination", "Loan origination", HH -> "+dL_h", Firms -> "+dL_f", Banks -> "-dL")(
          assets = Vector(AssetType.FirmLoan, AssetType.ConsumerLoan, AssetType.MortgageLoan, AssetType.NbfiLoan),
          mechanisms = Vector(
            FlowMechanism.HhCcOrigination,
            FlowMechanism.FirmNewLoan,
            FlowMechanism.MortgageOrigination,
            FlowMechanism.NbfiOrigination,
            FlowMechanism.QuasiFiscalLending,
          ),
        ),
        row("loan-repayment-default", "Loan repayment and defaults", HH -> "-repL_h", Firms -> "-repL_f", Banks -> "+repL")(
          assets = Vector(AssetType.FirmLoan, AssetType.ConsumerLoan, AssetType.MortgageLoan, AssetType.NbfiLoan),
          mechanisms = Vector(
            FlowMechanism.HhCcDebtService,
            FlowMechanism.HhCcDefault,
            FlowMechanism.FirmLoanRepayment,
            FlowMechanism.FirmNplDefault,
            FlowMechanism.MortgageRepayment,
            FlowMechanism.MortgageDefault,
            FlowMechanism.NbfiRepayment,
            FlowMechanism.NbfiDefault,
            FlowMechanism.QuasiFiscalRepayment,
          ),
        ),
        row(
          "bond-issuance-purchase",
          "Bond issuance and purchases",
          Firms -> "+dB_c",
          Banks -> "-dB_b",
          Gov   -> "+dB_g",
          NBP   -> "-dB_{nbp}",
          Ins   -> "-dB_i",
          Funds -> "-dB_f",
          ROW   -> "-dB_{row}",
        )(
          assets = Vector(AssetType.GovBondHTM, AssetType.GovBondAFS, AssetType.QuasiFiscalBond, AssetType.CorpBond),
          mechanisms = Vector(
            FlowMechanism.GovBondPrimaryMarket,
            FlowMechanism.GovBondForeignPurchase,
            FlowMechanism.NbpQeGovBondPurchase,
            FlowMechanism.InsuranceGovBondPurchase,
            FlowMechanism.TfiGovBondPurchase,
            FlowMechanism.QuasiFiscalBondIssuance,
            FlowMechanism.QuasiFiscalBondAmortization,
            FlowMechanism.QuasiFiscalNbpAbsorption,
            FlowMechanism.QuasiFiscalNbpBondAmortization,
            FlowMechanism.CorpBondIssuance,
            FlowMechanism.CorpBondAmortization,
          ),
        ),
        row("deposit-change", "Deposit change", HH -> "-dD_h", Firms -> "-dD_f", Banks -> "+dD", Funds -> "-dD_{fnd}")(
          assets = Vector(AssetType.DemandDeposit, AssetType.TermDeposit),
          mechanisms = Vector(
            FlowMechanism.InvestmentDepositSettlement,
            FlowMechanism.TfiDepositDrain,
            FlowMechanism.QuasiFiscalLendingDeposit,
            FlowMechanism.QuasiFiscalRepaymentDeposit,
          ),
        ),
      ),
    )

  val matrices: Vector[SymbolicMatrix] =
    Vector(balanceSheet, transactionsFlow)

  def mappingRows: Vector[MappingRow] =
    matrices.flatMap: matrix =>
      matrix.rows.map: row =>
        MappingRow(
          matrix = matrix.name,
          rowKey = row.rowKey,
          rowLabel = row.label,
          symbols = sectors.flatMap(sector => row.cells.get(sector)) :+ row.zeroSymbol,
          assets = row.assets,
          mechanisms = row.mechanisms,
          note = row.note,
        )

end SfcSymbolicMatrices
