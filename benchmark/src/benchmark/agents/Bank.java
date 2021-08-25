/*
 * JMAB - Java Macroeconomic Agent Based Modeling Toolkit
 * Copyright (C) 2013 Alessandro Caiani and Antoine Godin
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 */
package benchmark.agents;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import jmab.agents.AbstractBank;
import jmab.agents.BaselIIIAgent;
import jmab.agents.BondDemander;
import jmab.agents.BondSupplier;
import jmab.agents.CreditDemander;
import jmab.agents.CreditSupplier;
import jmab.agents.DepositDemander;
import jmab.agents.DepositSupplier;
import jmab.agents.CoCoBondDemander;
import jmab.agents.CoCoBondSupplier;
import jmab.agents.InterestRateSetterWithTargets;
import jmab.agents.MacroAgent;
import jmab.agents.ProfitsTaxPayer;
import jmab.events.MacroTicEvent;
import jmab.population.MacroPopulation;
import jmab.simulations.MacroSimulation;
import jmab.simulations.TwoStepMarketSimulation;
import jmab.stockmatrix.Bond;
import jmab.stockmatrix.Cash;
import jmab.stockmatrix.Deposit;
import jmab.stockmatrix.Item;
import jmab.stockmatrix.Loan;
import jmab.stockmatrix.CoCoBond;
import jmab.strategies.BankruptcyStrategy;
import jmab.strategies.BondDemandStrategy;
import jmab.strategies.DividendsStrategy;
import jmab.strategies.FinanceStrategy;
import jmab.strategies.InterestRateStrategy;
import jmab.strategies.SpecificCreditSupplyStrategy;
import jmab.strategies.SupplyCreditStrategy;
import jmab.strategies.TaxPayerStrategy;
import jmab.strategies.CoCoBondPricingStrategy;
import net.sourceforge.jabm.agent.Agent;
import net.sourceforge.jabm.event.AgentArrivalEvent;
import net.sourceforge.jabm.event.RoundFinishedEvent;
import benchmark.StaticValues;

/**
 * @author Alessandro Caiani and Antoine Godin
 *
 */
@SuppressWarnings("serial")
public class Bank extends AbstractBank implements CreditSupplier, CreditDemander,
		DepositSupplier, ProfitsTaxPayer, BondDemander, CoCoBondSupplier, InterestRateSetterWithTargets, BaselIIIAgent {

	private double reserveInterestRate;
	private double advancesInterestRate;
	private double bankInterestRate;
	private double depositInterestRate;
	private double totalLoanSupply;
	private double advancesDemand;
	private int advancesLength;
	private int advancesAmortizationType;
	private double bondPrice;
	private int bondDemand;
	private double paramtrigger;
	protected double cocobondPrice;
	protected int cocobondMaturity;
	protected double cocobondInterestRate;
	protected double totInterestsCoCoBonds;
	private double targetedAdditionalTier1;
	private BondSupplier selectedBondSupplier;
	private double bondInterestRate;
	private double riskAversionC;
	private double riskAversionK;
	private double capitalRatio;
	private double liquidityRatio;
	protected double advancesInterests;
	protected double bondInterestReceived;
	protected double totInterestsLoans;
	protected double totInterestsDeposits;
	private double dividends;
	private double bailoutCost;
	//private double preTaxProfits;
	//private double profitsAfterTax;
	private double targetedLiquidityRatio;
	private double targetedCapitalAdequacyRatio;
	private double bankrupted;
	private double cocotriggered;
	
	protected boolean triggered=false;
	
	
	/* (non-Javadoc)
	 * @see jmab.agents.MacroAgent#onRoundFinished(net.sourceforge.jabm.event.RoundFinishedEvent)
	 */
	@Override
	public void onRoundFinished(RoundFinishedEvent event) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see jmab.agents.MacroAgent#initialiseCounterpart(net.sourceforge.jabm.agent.Agent, int)
	 */
	@Override
	public void initialiseCounterpart(Agent counterpart, int marketID) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see jmab.agents.TaxPayer#payTaxes(jmab.goods.Item)
	 */
	@Override
	public void payTaxes(Item account) {
		double nW=this.getNetWealth();
		double profitsPreTax = nW-this.getPassedValue(StaticValues.LAG_NETWEALTH, 1);
		this.addValue(StaticValues.LAG_PROFITPRETAX,profitsPreTax);
		TaxPayerStrategy strategy = (TaxPayerStrategy) this.getStrategy(StaticValues.STRATEGY_TAXES);
		double taxes=0;
		if (!this.defaulted){
		taxes=strategy.computeTaxes(); 
		}
		else{
			taxes=0;
		}
		this.addValue(StaticValues.LAG_TAXES, taxes);
		Item res = this.getItemStockMatrix(true, StaticValues.SM_RESERVES);
		res.setValue(res.getValue()-taxes);
		account.setValue(account.getValue()+taxes);
	}

	/* (non-Javadoc)
	 * @see jmab.agents.CreditSupplier#getInterestRate(jmab.agents.MacroAgent, double, int)
	 */
	@Override
	public double getInterestRate(int idLoanSM, MacroAgent creditDemander, double amount,
			int length) {
		//InterestRateStrategy strategy = (InterestRateStrategy)this.getStrategy(StaticValues.STRATEGY_LOANAGENTINTERESTRATE);
		//double agentIR=strategy.computeInterestRate(creditDemander,amount,length);
		//return Math.max(this.bankInterestRate+agentIR, this.bondInterestRate);
		return Math.max(this.bankInterestRate, this.bondInterestRate);
	}

	/* (non-Javadoc)
	 * @see jmab.agents.CreditSupplier#getLoanSupply(jmab.agents.MacroAgent, double)
	 */
	@Override
	public double getLoanSupply(int loansId, MacroAgent creditDemander, double required) {
		SpecificCreditSupplyStrategy strategy=(SpecificCreditSupplyStrategy) this.getStrategy(StaticValues.STRATEGY_SPECIFICCREDITSUPPLY);
		double specificLoanSupply=strategy.computeSpecificSupply(creditDemander, required);
		return specificLoanSupply;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.CreditSupplier#getDepositInterestRate(jmab.agents.MacroAgent, double)
	 */
	@Override
	public double getDepositInterestRate(MacroAgent creditDemander,
			double amount) {
		return this.depositInterestRate;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.SimpleAbstractAgent#onTicArrived(jmab.events.AgentTicEvent)
	 */
	@Override
	protected void onTicArrived(MacroTicEvent event) {
		switch(event.getTic()){
		case StaticValues.TIC_COMPUTEEXPECTATIONS:
			setBailoutCost(0);
			setCurrentNonPerformingLoans(StaticValues.SM_LOAN,0); // we delete non performing loans from previous period
			this.defaulted=false;
			this.triggered=false;
			computeExpectations();
			double depositsValue=0;
			for(Item i:this.getItemsStockMatrix(false, StaticValues.SM_DEP)){
				depositsValue+=i.getValue();
				}
			double reservesValue=0;
			for(Item i:this.getItemsStockMatrix(true, StaticValues.SM_RESERVES)){
				reservesValue+=i.getValue();
				}
			if (depositsValue==0){
				this.liquidityRatio=0;
			}
			else{
			this.liquidityRatio=reservesValue/depositsValue;
			}
			double outstandingLoans=0;
			for (Item i:this.getItemsStockMatrix(true, StaticValues.SM_LOAN)){
				outstandingLoans+=i.getValue();
			}
			//ALE HAI AGGIUNTO QUESTO IL 24/1/2015
			//if (Math.floor(outstandingLoans)==0){
				//this.capitalRatio=0;
			//}
			this.capitalRatio=this.getPassedValue(StaticValues.LAG_NETWEALTH, 1)/outstandingLoans;
			break;
		case StaticValues.TIC_DEPINTERESTS:
			payDepositInterests();
			break;
		case StaticValues.TIC_CREDITSUPPLY:
			determineBankGenericInterestRate();
			determineCreditSupply();
			break;
		case StaticValues.TIC_DEPOSITSUPPLY:
			determineDepositInterestRate();
			break;
		case StaticValues.TIC_ADVINTERESTS:
			payInterests();
			break;
		case StaticValues.TIC_BANKRUPTCY:
			determineBankruptcy();
			break;
		case StaticValues.TIC_BONDDEMAND:
			determineBondDemand();
			break;
		case StaticValues.TIC_DIVIDENDS:
			payDividends();
			break;
		case StaticValues.TIC_RESERVEDEMANDBOND:
			//determineAdvancesDemandBond();
			break;
		case StaticValues.TIC_RESERVEDEMANDBASEL:
			determineAdvancesDemandBasel();
			break;
		case StaticValues.TIC_COCOBONDINTERESTS:
			payCoCoBondInterests();
			break;
		case StaticValues.TIC_COCOBONDSUPPLY:
			determineCoCoBondsInterestRate();
			emitCoCoBonds();
			break;
		case StaticValues.TIC_COCOBONDPRICE:
			computeCoCoBondPrice();
			break;
		case StaticValues.TIC_UPDATEEXPECTATIONS: 
			updateExpectations();
			break;
		}

	}
	
	protected void payDividends(){
		double nW=this.getNetWealth();
		double profitsAfterTax = nW-this.getPassedValue(StaticValues.LAG_NETWEALTH, 1);
		this.addValue(StaticValues.LAG_PROFITAFTERTAX,profitsAfterTax);
		DividendsStrategy strategy=(DividendsStrategy)this.getStrategy(StaticValues.STRATEGY_DIVIDENDS);
		strategy.payDividends();
	}

	/**
	 * 
	 */
	private void determineBankruptcy() {
		double nW=this.getNetWealth();
		if(nW<0){
			BankruptcyStrategy strategy = (BankruptcyStrategy)this.getStrategy(StaticValues.STRATEGY_BANKRUPTCY);
			strategy.bankrupt();
			bankrupted = 0;
			this.defaulted=true;
			if (this.defaulted=true) {
			bankrupted += 1;
			}
			//this.dead=true;
			//this.unsubscribeFromEvents();
			
		}
	}

	/**
	 * Updates the various expectations the bank is making.
	 */
	private void updateExpectations() { 
		double nW=this.getNetWealth();
		this.addValue(StaticValues.LAG_NETWEALTH,nW);
		//this.addValue(StaticValues.LAG_PROFIT,profits);
		this.addValue(StaticValues.LAG_REMAININGCREDIT, this.totalLoanSupply);
		this.addValue(StaticValues.LAG_NONPERFORMINGLOANS, this.currentNonPerformingLoans);
		this.addValue(StaticValues.LAG_DEPOSITINTEREST, depositInterestRate);
		this.addValue(StaticValues.LAG_LOANINTEREST, bankInterestRate);
		this.addValue(StaticValues.LAG_COCOBONDINTEREST, cocobondInterestRate);
		this.addValue(StaticValues.LAG_COCOTRIGGERED, cocotriggered);
		
		CoCoBond coco = (CoCoBond)this.getItemStockMatrix(false, StaticValues.SM_COCOBONDS); 
		double nomCoco= coco.getValue();
		this.addValue(StaticValues.LAG_COCOISSUED, nomCoco);
		
		double div=this.getDividends();
		this.addValue(StaticValues.LAG_DIVIDENDS, div);
		double[] deposit = new double[1];
		deposit[0]=this.getNumericBalanceSheet()[1][StaticValues.SM_DEP];
		this.getExpectation(StaticValues.EXPECTATIONS_DEPOSITS).addObservation(deposit);
		this.cleanSM();
	}

	
	/**
	 * Updates the interest rate of coco bonds
	 */
	protected void determineCoCoBondsInterestRate() {
		InterestRateStrategy strategy = (InterestRateStrategy)this.getStrategy(StaticValues.STRATEGY_COCOBONDINTERESTRATE);
		this.cocobondInterestRate = strategy.computeInterestRate(null, 0, 0);
	}	
	
	protected void computeCoCoBondPrice() {
		CoCoBond ccb = (CoCoBond)this.getItemStockMatrix(false, StaticValues.SM_COCOBONDS);
		CoCoBondPricingStrategy strategy = (CoCoBondPricingStrategy )this.getStrategy(StaticValues.STRATEGY_COCOBONDPRICING);
		ccb.setPrice(strategy.computeCoCoBondPrice());
		this.setCoCoBondPrice(strategy.computeCoCoBondPrice());
		this.addValue(StaticValues.LAG_COCOBONDSPRICE, ccb.getPrice());
	}
	
	/**
	 * Emits CoCo bonds
	 */
	
	protected void emitCoCoBonds() {
		/*double capitalsValue=this.getNetWealth();*/
		double pastcapitalsValue=this.getPassedValue(StaticValues.LAG_NETWEALTH, 1);
		double sharecocos=this.getTargetedAdditionalTier1();
		double needcocos = 0;
		
		/*if (this.getLiquidityRatio() < paramtrigger) {*/
		cocotriggered =0;
		if (this.getCapitalRatio() < paramtrigger) {
			this.triggered=true;
			cocotriggered +=1;
			System.out.println("bank "+ this.getAgentId() +" triggered");
		}
		
		/*else if (this.getLiquidityRatio() >= paramtrigger) {*/
		else if (this.getCapitalRatio() >= paramtrigger) {
			this.triggered=false;
		}
		
		/*
		if (this.triggered=false ) {
			needcocos += pastcapitalsValue*sharecocos;
		}
		else if (this.triggered=true ) {
			needcocos = 0;
		}
		*/
		
		if (this.getCapitalRatio() >= paramtrigger) {
			needcocos += pastcapitalsValue*sharecocos;
		}
		else if (this.getCapitalRatio() < paramtrigger) {
			needcocos = 0;
		}
		
		
		int quantity = 0;
		if(needcocos>=0) 
			quantity += (int)Math.ceil(needcocos/50);
		else
			quantity = 0;
		
		CoCoBond remainingCoCoBonds = (CoCoBond)this.getItemStockMatrix(false, StaticValues.SM_COCOBONDS,this);
		if(remainingCoCoBonds!=null && this.getCapitalRatio() >= paramtrigger) {
		/*if(remainingCoCoBonds!=null && this.triggered == false) {*/
			quantity+=remainingCoCoBonds.getQuantity();
			this.removeItemStockMatrix(remainingCoCoBonds, false, StaticValues.SM_COCOBONDS);
		}
		else if (remainingCoCoBonds!=null && this.getCapitalRatio() < paramtrigger) {
		/*else if (remainingCoCoBonds!=null && this.triggered == true) {*/
			quantity=0;
			this.removeItemStockMatrix(remainingCoCoBonds, false, StaticValues.SM_COCOBONDS);
		}	
		CoCoBond newIssue = new CoCoBond(quantity*this.cocobondPrice, quantity, this, this, this.cocobondMaturity, this.cocobondInterestRate,
				this.cocobondPrice);
		this.addItemStockMatrix(newIssue, false, StaticValues.SM_COCOBONDS);
		this.setActive(true, StaticValues.MKT_COCOBONDS);
	}

	/**
	 * Pays cocobonds interests to their asset holder
	 */
	protected void payCoCoBondInterests() {
		List<Item> cocobonds=this.getItemsStockMatrix(false, StaticValues.SM_COCOBONDS);
		Item reserve=this.getItemStockMatrix(true, StaticValues.SM_RESERVES);
		double interestsCoCoBonds=0;
		for(Item b:cocobonds){
			CoCoBond cocobond=(CoCoBond)b;
			if(cocobond.getAssetHolder() instanceof Bank){
				cocobond.setQuantity(0);
			}else{
				CoCoBondDemander holder = (CoCoBondDemander)cocobond.getAssetHolder();
				holder.setCoCoBondInterestsReceived(cocobond.getValue()*cocobond.getInterestRate());
				interestsCoCoBonds+=cocobond.getValue()*cocobond.getInterestRate();
				if(holder instanceof Households){
					Item hhDep = holder.getItemStockMatrix(true, StaticValues.SM_DEP);
					reserve.setValue(reserve.getValue()-cocobond.getValue()*cocobond.getInterestRate()); /**banks' deposits are used to pay interests on cocos*/
					hhDep.setValue(hhDep.getValue()+cocobond.getValue()*cocobond.getInterestRate()); /**interests on cocos are transfered to households' deposits*/
					if(cocobond.getAge()==cocobond.getMaturity() && this.getCapitalRatio() >= paramtrigger){
					/*if(cocobond.getAge()==cocobond.getMaturity() && this.triggered == false){*/
						reserve.setValue(reserve.getValue()-cocobond.getValue()); /**when the coco's maturity is reached, banks' deposits are used to pay the principal*/
						hhDep.setValue(hhDep.getValue()+cocobond.getValue()); /**the principal is transfered to households' deposits*/
						cocobond.setQuantity(0); /**we ensure that these cocos aren't taken into account anymore*/
					}
					else if (cocobond.getAge()==cocobond.getMaturity() && this.getCapitalRatio() < paramtrigger) {
					/*else if (cocobond.getAge()==cocobond.getMaturity() && this.triggered == true) {*/
						cocobond.setQuantity(0); /**we ensure that these cocos aren't taken into account anymore*/
					}
					else if (this.getCapitalRatio() < paramtrigger) {
						cocobond.setQuantity(0);
					}
				}
			}
		}
		totInterestsCoCoBonds=interestsCoCoBonds;
		this.addValue(StaticValues.LAG_COCOINTERESTSPAID,interestsCoCoBonds);
		
	}
	
	
	/**
	 * Determines the bank generic interest rate, i.e. the fixed mark-up on the basis interest rate. 
	 */
	private void determineBankGenericInterestRate() {
		InterestRateStrategy strategy = (InterestRateStrategy)this.getStrategy(StaticValues.STRATEGY_LOANBANKINTERESTRATE);
		this.bankInterestRate=strategy.computeInterestRate(null,0,0);
		
	}

	/**
	 * Determines the advances to be requested to the central bank in order to meet BaselII requirements
	 */
	private void determineAdvancesDemandBasel() {
		FinanceStrategy strategy = (FinanceStrategy)this.getStrategy(StaticValues.STRATEGY_ADVANCES);
		this.advancesDemand=strategy.computeCreditDemand(0);//see BaselIIReserveRequirements strategy
		if(this.advancesDemand>0)
			this.setActive(true, StaticValues.MKT_ADVANCES);
	}

//	/**
//	 * Determines the advances to be requested to the central bank in order to be able to buy bonds
//	 */
//	private void determineAdvancesDemandBond() {
//		FinanceStrategy strategy = (FinanceStrategy)this.getStrategy(StaticValues.STRATEGY_FINANCE);
//		this.advancesDemand=strategy.computeCreditDemand(bondDemand*bondPrice-this.getItemStockMatrix(true, StaticValues.SM_RESERVES).getValue());
//		if(this.advancesDemand>0)
//			this.setActive(true, StaticValues.MKT_ADVANCES);
//	}

	private void payInterests() {
		List<Item> loans=this.getItemsStockMatrix(false, StaticValues.SM_ADVANCES);
		Deposit deposit = (Deposit) this.getItemStockMatrix(true, StaticValues.SM_RESERVES);
		this.advancesInterests = 0;
		for(int i=0;i<loans.size();i++){
			Loan loan=(Loan)loans.get(i);
			if(loan.getAge()>0){
				double iRate=loan.getInterestRate();
				double amount=loan.getInitialAmount();
				int length = loan.getLength();
				double interests=iRate*loan.getValue();
				double principal=0.0;
				switch(loan.getAmortization()){
				case Loan.FIXED_AMOUNT:
					double amortization = amount*(iRate*Math.pow(1+iRate, length))/(Math.pow(1+iRate, length)-1);
					principal=amortization-interests;
					break;
				case Loan.FIXED_CAPITAL:
					principal=amount/length;
					break;
				case Loan.ONLY_INTERESTS:
					if(length==loan.getAge())
						principal=amount;
					break;
				}
				this.advancesInterests+=interests;
				double amountToPay=interests+principal;
				deposit.setValue(deposit.getValue()-amountToPay);
				loan.setValue(loan.getValue()-principal);
			}
		}
	}
	
	/**
	 * What to do when the bank is awaken by the arrival of an event (market-based events)
	 * @param event the event that awoke the agent
	 */
	public void onAgentArrival(AgentArrivalEvent event) {
		MacroSimulation macroSim = (MacroSimulation)event.getSimulationController().getSimulation();
		int marketID=macroSim.getActiveMarket().getMarketId();
		switch(marketID){
		case StaticValues.MKT_BONDS:
			TwoStepMarketSimulation sim = (TwoStepMarketSimulation)macroSim.getActiveMarket();
			if(sim.isFirstStep()){
				this.selectedBondSupplier=(BondSupplier)event.getObjects().get(0);
				this.bondPrice=this.selectedBondSupplier.getBondPrice();
				this.bondInterestRate=this.selectedBondSupplier.getBondInterestRate();
			}else if(sim.isSecondStep())
				macroSim.getActiveMarket().commit(this, this.selectedBondSupplier,marketID);
			break;
		case StaticValues.MKT_ADVANCES:
			macroSim.getActiveMarket().commit(this, (MacroAgent)event.getObjects().get(0),marketID);
			break;
		}
	}
	
	/**
	 * Determines the demand for bonds
	 */
	private void determineBondDemand() {
		BondDemandStrategy strategy = (BondDemandStrategy)this.getStrategy(StaticValues.STRATEGY_BONDDEMAND);
		this.bondDemand=strategy.bondDemand(this.selectedBondSupplier);
		if (this.bondDemand>0){
			this.setActive(true, StaticValues.MKT_BONDS);
		}
	}

	/**
	 * Determines the interest rate that is offered on deposits
	 */
	private void determineDepositInterestRate() {
		InterestRateStrategy strategy = (InterestRateStrategy)this.getStrategy(StaticValues.STRATEGY_DEPOSITINTERESTRATE);
		this.depositInterestRate=strategy.computeInterestRate(null, 0, 0);
		this.setActive(true, StaticValues.MKT_DEPOSIT);
	}

	/**
	 * Determines the total credit supplied
	 */
	private void determineCreditSupply() {
		SupplyCreditStrategy strategy=(SupplyCreditStrategy)this.getStrategy(StaticValues.STRATEGY_CREDITSUPPLY);
		double loanSupply=strategy.computeCreditSupply();
		setTotalLoansSupply(StaticValues.SM_LOAN, loanSupply);
		this.addValue(StaticValues.LAG_BANKTOTLOANSUPPLY, loanSupply);
		if (this.getTotalLoansSupply(StaticValues.SM_LOAN)>0){
			this.setActive(true, StaticValues.MKT_CREDIT);
		}
		
	}

	/**
	 * Pays interest rates to all deposit holders. Note that there is no counterpart flow.
	 */
	private void payDepositInterests() {
		List<Item> deposits = this.getItemsStockMatrix(false, StaticValues.SM_DEP);
		double totInterests=0;
		for(Item d:deposits){
			Deposit dep = (Deposit)d;
			DepositDemander depositor = (DepositDemander)dep.getAssetHolder();
			depositor.interestPaid(dep.getInterestRate()*dep.getValue());
			totInterests+=dep.getInterestRate()*dep.getValue();
			dep.setValue(dep.getValue()*(1+dep.getInterestRate()));	
		}
		totInterestsDeposits=totInterests;
		
	}

	/* (non-Javadoc)
	 * @see jmab.agents.CreditDemander#getLoanRequirement()
	 */
	@Override
	public double getLoanRequirement(int idLoanSM) {
		return advancesDemand;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.CreditDemander#decideLoanLength()
	 */
	@Override
	public int decideLoanLength(int idLoanSM) {
		return this.advancesLength;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.CreditDemander#decideLoanAmortization()
	 */
	@Override
	public int decideLoanAmortizationType(int idLoanSM) {
		return this.advancesAmortizationType;
	}

	/**
	 * @return the reserveLength
	 */
	public int getAdvancesLength() {
		return advancesLength;
	}

	/**
	 * @param reserveLength the reserveLength to set
	 */
	public void setAdvancesLength(int advancesLength) {
		this.advancesLength = advancesLength;
	}

	/**
	 * @return the reserveAmortization
	 */
	public int getAdvancesAmortizationType() {
		return advancesAmortizationType;
	}

	/**
	 * @param reserveAmortization the reserveAmortization to set
	 */
	public void setAdvancesAmortizationType(int advancesAmortizationType) {
		this.advancesAmortizationType = advancesAmortizationType;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.BondDemander#getBondsDemand(double)
	 */
	@Override
	public int getBondsDemand(double price, BondSupplier issuer) {
		return bondDemand;
	}
	public int getBondsDemand(){
		return bondDemand;
	}

	/**
	 * Implements the bond demand paying stocks. Since we assume banks to buy all bonds, need to make sure
	 * there is enough money on the reserve account. Thus allow cash to go negative if needed. Anyway the reserve market
	 * afterwards will make sure there is no negative values for cash and reserves.
	 * TODO: CHECK THIS
	 * @see jmab.agents.BondDemander#getPayingStocks(int, jmab.stockmatrix.Item)
	 */
	@Override
	public List<Item> getPayingStocks(int idBondSM, Item payableStock) {
		List<Item> result = new ArrayList<Item>();
		result.addAll(this.getItemsStockMatrix(true, StaticValues.SM_RESERVES));
		return result;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.CreditDemander#setLoanRequirement(double)
	 */
	@Override
	public void setLoanRequirement(int idLoanSM, double d) {
		this.advancesDemand=d;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.CreditSupplier#getTotalLoansSupply()
	 */
	@Override
	public double getTotalLoansSupply(int loansId) {
		return this.totalLoanSupply;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.CreditSupplier#setTotalLoansSupply(double)
	 */
	@Override
	public void setTotalLoansSupply(int loansId, double d) {
		this.totalLoanSupply=d;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.InterestRateSetterWithTargets#getInterestRate()
	 */
	@Override
	public double getInterestRate(int mktId) {
		switch(mktId){
		case StaticValues.MKT_CREDIT:
			return this.bankInterestRate;
		case StaticValues.MKT_DEPOSIT:
			return this.depositInterestRate;
		case StaticValues.MKT_COCOBONDS:
			return this.cocobondInterestRate;
		}
		return 0;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.InterestRateSetterWithTargets#getInterestRateLowerBound()
	 */
	@Override
	public double getInterestRateLowerBound(int mktId) {
		switch(mktId){
		case StaticValues.MKT_CREDIT:
			return (0);
			//return (0-this.advancesInterestRate);
		case StaticValues.MKT_DEPOSIT:
			return 0;
		case StaticValues.MKT_COCOBONDS:
			return 0;
		}
		return 0;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.InterestRateSetterWithTargets#getReferenceVariableForInterestRate()
	 */
	@Override
	public double getReferenceVariableForInterestRate(int mktId) {
		switch(mktId){
		case StaticValues.MKT_CREDIT:
			double outstandingLoans=0;
			for (Item i:this.getItemsStockMatrix(true, StaticValues.SM_LOAN)){
				outstandingLoans+=i.getValue();
			}
			if (outstandingLoans==0){
//TODO				System.out.println("CR (0.1799): NO LOANS");
				return Double.POSITIVE_INFINITY;
			}
			else{
//TODO				System.out.println("CR (0.1799): " + this.getPassedValue(StaticValues.LAG_NETWEALTH, 1)/outstandingLoans );
				return this.getPassedValue(StaticValues.LAG_NETWEALTH, 1)/outstandingLoans;
			}
			
			
		case StaticValues.MKT_DEPOSIT:
			double depositsValue=0;
			for(Item i:this.getItemsStockMatrix(false, StaticValues.SM_DEP)){
				depositsValue+=i.getValue();
				}
			double reservesValue=0;
			for(Item i:this.getItemsStockMatrix(true, StaticValues.SM_RESERVES)){
				reservesValue+=i.getValue();
				}
			if (depositsValue==0){
//TODO				System.out.println("LR(0.25802): DEPOSITS 0");
				return 0;
			}
			else{
//TODO			System.out.println("LR(0.25802): " + reservesValue/depositsValue );
			return reservesValue/depositsValue;
			}

		case StaticValues.MKT_COCOBONDS:
			double depositsValue1=0;
			for(Item i:this.getItemsStockMatrix(false, StaticValues.SM_DEP)){
				depositsValue1+=i.getValue();
				}
			double reservesValue1=0;
			for(Item i:this.getItemsStockMatrix(true, StaticValues.SM_RESERVES)){
				reservesValue1+=i.getValue();
				}
			if (depositsValue1==0){
				return 0;
			}
			else{
			return reservesValue1/depositsValue1;
			}
			
			}
		return Double.NaN;
	}
		/* OLD VERSION
		switch(mktId){
		case StaticValues.MKT_CREDIT:
			return this.getPassedValue(StaticValues.LAG_REMAININGCREDIT, 1)/this.getPassedValue(StaticValues.LAG_BANKTOTLOANSUPPLY, 1);
		case StaticValues.MKT_DEPOSIT:
			return this.getPassedValue(StaticValues.LAG_REMAININGCREDIT, 1)/this.getPassedValue(StaticValues.LAG_BANKTOTLOANSUPPLY, 1);
		}
		return Double.NaN;	
	}
*/
	/* (non-Javadoc)
	 * @see jmab.agents.InterestRateSetterWithTargets#getInterestRateUpperBound(int)
	 */
	@Override
	public double getInterestRateUpperBound(int mktId) {
		switch(mktId){
		case StaticValues.MKT_CREDIT:
			return Double.POSITIVE_INFINITY;
		case StaticValues.MKT_DEPOSIT:
			return this.advancesInterestRate;
		case StaticValues.MKT_COCOBONDS:
			return Double.POSITIVE_INFINITY;	
		}
		return 0;
	}

	/**
	 * @return the reserveInterestRate
	 */
	public double getReserveInterestRate() {
		return reserveInterestRate;
	}

	/**
	 * @param reserveInterestRate the reserveInterestRate to set
	 */
	public void setReserveInterestRate(double reserveInterestRate) {
		this.reserveInterestRate = reserveInterestRate;
	}

	/**
	 * @return the bankInterestRate
	 */
	public double getBankInterestRate() {
		return bankInterestRate;
	}

	/**
	 * @param bankInterestRate the bankInterestRate to set
	 */
	public void setBankInterestRate(double bankInterestRate) {
		this.bankInterestRate = bankInterestRate;
	}

	/**
	 * @return the bankInterestRate
	 */
	public double getBankrupted() {
		return bankrupted;
	}
	
	public double getCocotriggered() {
		return cocotriggered;
	}
	
	public void setCocotriggered(double cocotriggered) {
		this.cocotriggered = cocotriggered;
	}	

	public boolean isTriggered(){
		return triggered;
	}

	
	/**
	 * @param bankInterestRate the bankInterestRate to set
	 */
	public void setBankrupted(double bankrupted) {
		this.bankrupted = bankrupted;
	}	
	
	
	/**
	 * @return the depositInterestRate
	 */
	public double getDepositInterestRate() {
		return depositInterestRate;
	}

	/**
	 * @param depositInterestRate the depositInterestRate to set
	 */
	public void setDepositInterestRate(double depositInterestRate) {
		this.depositInterestRate = depositInterestRate;
	}
	
	/**
	 * @return the cocobondInterestRate
	 */
	public double getCoCoBondInterestRate() {
		return cocobondInterestRate;
	}
	
	/**
	 * @param cocobondInterestRate the cocobondInterestRate to set
	 */
	public void setCoCoBondInterestRate(double cocobondInterestRate) {
		this.cocobondInterestRate = cocobondInterestRate;
	}
	
	@Override
	public double getCoCoBondInterestRate(MacroAgent cocobondDemander, double amount) {
		return cocobondInterestRate;
	}
	
	/**
	 * @return the cocobondPrice
	 */
	public double getCoCoBondPrice() {
		return cocobondPrice;
	}

	/**
	 * @param bondValue the cocobondPrice to set
	 */
	public void setCoCoBondPrice(double cocobondPrice) {
		this.cocobondPrice = cocobondPrice;
	}
	
	/**
	 * @return the cocobondMaturity
	 */
	public int getCoCoBondMaturity() {
		return cocobondMaturity;
	}

	/**
	 * @param cocobondMaturity the cocobondMaturity to set
	 */
	public void setCoCoBondMaturity(int cocobondMaturity) {
		this.cocobondMaturity = cocobondMaturity;
	}

	@Override
	public Item getPayableStock1(int idCoCoBondSM) {
		return this.getItemStockMatrix(true, StaticValues.SM_RESERVES);
	}
	
	@Override
	public int getCoCoBondSupply() {
		return (int) this.getItemStockMatrix(false, StaticValues.SM_COCOBONDS,this).getQuantity();
	}
	
	/**
	 * @return the totInterestsCoCoBonds
	 */
	public double getTotInterestsCoCoBonds() {
		return totInterestsCoCoBonds;
	}

	/**
	 * @param totInterestsCoCoBonds the totInterestsCoCoBonds to set
	 */
	public void setTotInterestsCoCoBonds(double totInterestsCoCoBonds) {
		this.totInterestsCoCoBonds = totInterestsCoCoBonds;
	}
	
	public double getParamTrigger() {
		return this.paramtrigger;
	}

	public void setParamTrigger(double paramtrigger) {
		this.paramtrigger = paramtrigger;
	}
	
	@Override
	public double getTargetedAdditionalTier1() {
		return this.targetedAdditionalTier1;
	}

	public void setTargetedAdditionalTier1(double targetedAdditionalTier1) {
		this.targetedAdditionalTier1 = targetedAdditionalTier1;
	}

	/**
	 * @return the reserveDemand
	 */
	public double getReserveDemand() {
		return advancesDemand;
	}

	/**
	 * @param reserveDemand the reserveDemand to set
	 */
	public void setReserveDemand(double reserveDemand) {
		this.advancesDemand = reserveDemand;
	}

	/**
	 * @return the bondPrice
	 */
	public double getBondPrice() {
		return bondPrice;
	}

	/**
	 * @param bondPrice the bondPrice to set
	 */
	public void setBondPrice(double bondPrice) {
		this.bondPrice = bondPrice;
	}

	/**
	 * @return the bondDemand
	 */
	public int getBondDemand() {
		return bondDemand;
	}

	/**
	 * @param bondDemand the bondDemand to set
	 */
	public void setBondDemand(int bondDemand) {
		this.bondDemand = bondDemand;
	}

	/**
	 * @return the bondInterestRate
	 */
	public double getBondInterestRate() {
		return bondInterestRate;
	}

	/**
	 * @param bondInterestRate the bondInterestRate to set
	 */
	public void setBondInterestRate(double bondInterestRate) {
		this.bondInterestRate = bondInterestRate;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.ProfitsTaxPayer#getProfits()
	 */
	@Override
	public double getPreTaxProfits() {
		if(!dead)
			return this.getPassedValue(StaticValues.LAG_PROFITPRETAX, 0);
		else
			return Double.NaN;
	}

	/**
	 * @return the advancesInterestRate
	 */
	public double getAdvancesInterestRate() {
		return advancesInterestRate;
	}

	/**
	 * @param advancesInterestRate the advancesInterestRate to set
	 */
	public void setAdvancesInterestRate(double advancesInterestRate) {
		this.advancesInterestRate = advancesInterestRate;
	}

	/**
	 * @return the riskAversion
	 */
	public double getRiskAversion(MacroAgent creditDemander) {
		if (creditDemander instanceof ConsumptionFirm){
			return riskAversionC;
		}
		else{
			return riskAversionK;
		}
	}

	/**
	 * @param riskAversion the riskAversion to set
	 */
	public void setRiskAversionC(double riskAversion) {
		this.riskAversionC = riskAversion;
	}
	public void setRiskAversionK(double riskAversion) {
		this.riskAversionK = riskAversion;
	}

	/**
	 * @return the capitalRatio
	 */
	public double getCapitalRatio() {
		return capitalRatio;
	}

	/**
	 * @param capitalRatio the capitalRatio to set
	 */
	public void setCapitalRatio(double capitalRatio) {
		this.capitalRatio = capitalRatio;
	}

	/**
	 * @return the liquidityRatio
	 */
	public double getLiquidityRatio() {
		return liquidityRatio;
	}

	/**
	 * @param liquidityRatio the liquidityRatio to set
	 */
	public void setLiquidityRatio(double liquidityRatio) {
		this.liquidityRatio = liquidityRatio;
	}

	/**
	 * @return the advancesInterests
	 */
	public double getAdvancesInterests() {
		return advancesInterests;
	}

	/**
	 * @param advancesInterests the advancesInterests to set
	 */
	public void setAdvancesInterests(double advancesInterests) {
		this.advancesInterests = advancesInterests;
	}

	/**
	 * @return the bondInterestReceived
	 */
	public double getBondInterestReceived() {
		return bondInterestReceived;
	}

	/**
	 * @param bondInterestReceived the bondInterestReceived to set
	 */
	public void setBondInterestReceived(double bondInterestReceived) {
		this.bondInterestReceived = bondInterestReceived;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.BondDemander#setBondInterestsReceived(double)
	 */
	@Override
	public void setBondInterestsReceived(double interests) {
		this.bondInterestReceived = interests;
		
	}


	/**
	 * @return the totInterestsLoans
	 */
	public double getTotInterestsLoans() {
		return totInterestsLoans;
	}

	/**
	 * @return the dividends
	 */
	public double getDividends() {
		return dividends;
	}

	/**
	 * @param dividends the dividends to set
	 */
	public void setDividends(double dividends) {
		this.dividends = dividends;
	}

	/**
	 * @param totInterestsLoans the totInterestsLoans to set
	 */
	public void setTotInterestsLoans(double totInterestsLoans) {
		this.totInterestsLoans = totInterestsLoans;
	}

	/**
	 * @return the totInterestsDeposits
	 */
	public double getTotInterestsDeposits() {
		return totInterestsDeposits;
	}

	/**
	 * @param totInterestsDeposits the totInterestsDeposits to set
	 */
	public void setTotInterestsDeposits(double totInterestsDeposits) {
		this.totInterestsDeposits = totInterestsDeposits;
	}

	/**
	 * @return the bailoutCost
	 */
	public double getBailoutCost() {
		return bailoutCost;
	}

	/**
	 * @param bailoutCost the bailoutCost to set
	 */
	public void setBailoutCost(double bailoutCost) {
		this.bailoutCost = bailoutCost;
	}
	
	

	/**
	 * Populates the agent characteristics using the byte array content. The structure is as follows:
	 * [sizeMacroAgentStructure][MacroAgentStructure][reserveIR][advancesIR][bankIR][depositIR][bondsIR][loanSupply][advancesDemand]
	 * [bondPrice][riskAversionC][riskAversionK][capitalRatio][liquidityRatio][advancesInterests][bondInterestsReceived]
	 * [advancesLenght][advancesAmortizationType][bondDemand][bondSupplierPopulationId][bondSupplierId][matrixSize][stockMatrixStructure]
	 * [expSize][ExpectationStructure][passedValSize][PassedValStructure][stratsSize][StrategiesStructure]
	 */
	@Override
	public void populateAgent(byte[] content, MacroPopulation pop) {
		ByteBuffer buf = ByteBuffer.wrap(content);
		byte[] macroBytes = new byte[buf.getInt()];
		buf.get(macroBytes);
		super.populateCharacteristics(macroBytes, pop);
		reserveInterestRate = buf.getDouble();
		advancesInterestRate = buf.getDouble();
		bankInterestRate = buf.getDouble();
		depositInterestRate = buf.getDouble();
		bondInterestRate = buf.getDouble();
		totalLoanSupply = buf.getDouble();
		advancesDemand = buf.getDouble();
		bondPrice = buf.getDouble();
		cocobondPrice = buf.getDouble();
		riskAversionC = buf.getDouble();
		riskAversionK = buf.getDouble();
		capitalRatio = buf.getDouble();
		liquidityRatio = buf.getDouble();
		advancesInterests = buf.getDouble();
		bondInterestReceived = buf.getDouble();
		cocobondInterestRate = buf.getDouble();
		targetedAdditionalTier1 = buf.getDouble();
		paramtrigger = buf.getDouble();
		cocobondMaturity = buf.getInt();
		advancesLength = buf.getInt();
		advancesAmortizationType = buf.getInt();
		bondDemand = buf.getInt();
		this.triggered = buf.get()==1;

		Collection<Agent> aHolders = pop.getPopulation(buf.getInt()).getAgents();
		long selSupplierId = buf.getLong(); 
		for(Agent a:aHolders){
			MacroAgent pot = (MacroAgent) a;
			if(pot.getAgentId()==selSupplierId){
				this.selectedBondSupplier=(BondSupplier)pot;
				break;
			}
		}
		int matSize = buf.getInt();
		if(matSize>0){
			byte[] smBytes = new byte[matSize];
			buf.get(smBytes);
			this.populateStockMatrixBytes(smBytes, pop);
		}
		int expSize = buf.getInt();
		if(expSize>0){
			byte[] expBytes = new byte[expSize];
			buf.get(expBytes);
			this.populateExpectationsBytes(expBytes);
		}
		int lagSize = buf.getInt();
		if(lagSize>0){
			byte[] lagBytes = new byte[lagSize];
			buf.get(lagBytes);
			this.populatePassedValuesBytes(lagBytes);
		}
		int stratSize = buf.getInt();
		if(stratSize>0){
			byte[] stratBytes = new byte[stratSize];
			buf.get(stratBytes);
			this.populateStrategies(stratBytes, pop);
		}
	}

	/**
	 * Generates the byte array containing all relevant informations regarding the bank agent. The structure is as follows:
	 * [sizeMacroAgentStructure][MacroAgentStructure][reserveIR][advancesIR][bankIR][depositIR][bondsIR][loanSupply][advancesDemand]
	 * [bondPrice][riskAversionC][riskAversionK][capitalRatio][liquidityRatio][advancesInterests][bondInterestsReceived]
	 * [advancesLenght][advancesAmortizationType][bondDemand][bondSupplierPopId][bondSupplierId][matrixSize][stockMatrixStructure]
	 * [expSize][ExpectationStructure][passedValSize][PassedValStructure][stratsSize][StrategiesStructure]
	 */
	@Override
	public byte[] getBytes() {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		try {
			byte[] charBytes = super.getAgentCharacteristicsBytes();
			out.write(ByteBuffer.allocate(4).putInt(charBytes.length).array());
			out.write(charBytes);
			ByteBuffer buf = ByteBuffer.allocate(136);
			buf.putDouble(reserveInterestRate);
			buf.putDouble(advancesInterestRate);
			buf.putDouble(bankInterestRate);
			buf.putDouble(depositInterestRate);
			buf.putDouble(bondInterestRate);
			buf.putDouble(totalLoanSupply);
			buf.putDouble(advancesDemand);
			buf.putDouble(bondPrice);
			buf.putDouble(cocobondPrice);
			buf.putDouble(riskAversionC);
			buf.putDouble(riskAversionK);
			buf.putDouble(capitalRatio);
			buf.putDouble(liquidityRatio);
			buf.putDouble(advancesInterests);
			buf.putDouble(bondInterestReceived);
			buf.putDouble(cocobondInterestRate);
			buf.putDouble(targetedAdditionalTier1);
			buf.putDouble(paramtrigger);
			buf.putInt(cocobondMaturity);
			buf.putInt(advancesLength);
			if(triggered)
				buf.put((byte)1);
			else
				buf.put((byte)0);
			buf.putInt(advancesAmortizationType);
			buf.putInt(bondDemand);
			buf.putInt(selectedBondSupplier.getPopulationId());
			buf.putLong(selectedBondSupplier.getAgentId());
			out.write(buf.array());
			byte[] smBytes = super.getStockMatrixBytes();
			out.write(ByteBuffer.allocate(4).putInt(smBytes.length).array());
			out.write(smBytes);
			byte[] expBytes = super.getExpectationsBytes();
			out.write(ByteBuffer.allocate(4).putInt(expBytes.length).array());
			out.write(expBytes);
			byte[] passedValBytes = super.getPassedValuesBytes();
			out.write(ByteBuffer.allocate(4).putInt(passedValBytes.length).array());
			out.write(passedValBytes);
			byte[] stratsBytes = super.getStrategiesBytes();
			out.write(ByteBuffer.allocate(4).putInt(stratsBytes.length).array());
			out.write(stratsBytes);
		} catch (IOException e) {
			e.printStackTrace();
		}
		return out.toByteArray();
	}

	/**
	 * Populates the stockMatrix with the byte array content. The structure of the stock matrix is the following:
	 * [nbStockTypes]
	 * for each type of stocks
	 * 	[IdStock][nbItems]
	 * 		for each Item
	 * 			[itemSize][itemStructure]
	 * 		end for
	 * end for 	
	 */
	@Override
	public void populateStockMatrixBytes(byte[] content, MacroPopulation pop) {
		ByteBuffer buf = ByteBuffer.wrap(content);
		int nbStockTypes = buf.getInt();
		for(int i = 0 ; i < nbStockTypes ; i++){
			int stockId = buf.getInt();
			int nbStocks = buf.getInt();
			for(int j = 0 ; j < nbStocks ; j++){
				int itemSize = buf.getInt();
				byte[] itemData = new byte[itemSize];
				buf.get(itemData);
				Item it;
				switch(stockId){
				case StaticValues.SM_ADVANCES:
					it = new Loan(itemData, pop, this);
					break;
				case StaticValues.SM_BONDS:
					it = new Bond(itemData, pop, this);
					break;
				case StaticValues.SM_CASH:
					it = new Cash(itemData, pop, this);
					break;
//				case StaticValues.SM_DEP:
//					it = new Deposit(itemData, pop, this);
//					break;
				case StaticValues.SM_LOAN:
					it = new Loan(itemData, pop, this);
					break;
				default:
					it = new Deposit(itemData, pop, this);
					break;
				case StaticValues.SM_COCOBONDS:
					it = new CoCoBond(itemData, pop, this);
					break;
				}
				this.addItemStockMatrix(it, true, stockId);
				MacroAgent liabHolder = it.getLiabilityHolder();
				liabHolder.addItemStockMatrix(it, false, stockId);
			}
		}	
	}

	/**
	 * @return the riskAversionC
	 */
	public double getRiskAversionC() {
		return riskAversionC;
	}

	/**
	 * @return the riskAversionK
	 */
	public double getRiskAversionK() {
		return riskAversionK;
	}

	@Override
	public double getTargetedLiquidityRatio() {
		return this.targetedLiquidityRatio;
	}

	public void setTargetedLiquidityRatio(double targetedLiquidityRatio) {
		this.targetedLiquidityRatio = targetedLiquidityRatio;
	}
	
	@Override
	public double getTargetedCapitalAdequacyRatio() {
		return this.targetedCapitalAdequacyRatio;
	}

	public void setTargetedCapitalAdequacyRatio(double targetedCapitalAdequacyRatio) {
		this.targetedCapitalAdequacyRatio = targetedCapitalAdequacyRatio;
	}
}
