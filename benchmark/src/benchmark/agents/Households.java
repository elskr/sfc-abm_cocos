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

import jmab.agents.AbstractHousehold;
import jmab.agents.DepositDemander;
import jmab.agents.GoodDemander;
import jmab.agents.CoCoBondDemander;
import jmab.agents.CoCoBondSupplier;
import jmab.agents.IncomeTaxPayer;
import jmab.agents.LaborSupplier;
import jmab.agents.MacroAgent;
import jmab.agents.WageSetterWithTargets;
import jmab.events.MacroTicEvent;
import jmab.population.MacroPopulation;
import jmab.simulations.TwoStepMarketSimulation;
import jmab.simulations.MacroSimulation;
import jmab.stockmatrix.Cash;
import jmab.stockmatrix.ConsumptionGood;
import jmab.stockmatrix.Deposit;
import jmab.stockmatrix.CoCoBond;
import jmab.stockmatrix.Item;
import jmab.strategies.ConsumptionStrategy;
import jmab.strategies.CoCoBondDemandStrategy;
import jmab.strategies.SelectDepositSupplierStrategy;
import jmab.strategies.SelectSellerStrategy;
import jmab.strategies.SelectCoCoBondSupplierStrategy;
import jmab.strategies.TaxPayerStrategy;
import jmab.strategies.WageStrategy;
import net.sourceforge.jabm.Population;
import net.sourceforge.jabm.SimulationController;
import net.sourceforge.jabm.agent.Agent;
import net.sourceforge.jabm.event.AgentArrivalEvent;
import net.sourceforge.jabm.event.RoundFinishedEvent;
import benchmark.StaticValues;


/**
 * @author Alessandro Caiani and Antoine Godin
 *
 */
@SuppressWarnings("serial")
public class Households extends AbstractHousehold implements GoodDemander, LaborSupplier,
		DepositDemander, IncomeTaxPayer, WageSetterWithTargets, CoCoBondDemander {
	
	private double demand;
	private double cashAmount;
	private double depositAmount;
	private int employmentWageLag;
	protected double shareDeposits;
	/*private CoCoBondSupplier selectedCoCoBondSupplier;*/
	protected ArrayList<Agent> selectedCoCoBondSuppliers;
	private double cocobondPrice;
	private int cocobondnominalDemand;
	private double cocobondInterestRate;
	protected double cocobondInterestReceived;
	protected double interestsReceived;
	protected double dividendsReceived;
	protected double cocoshare;
	

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

	
	/**
	 * @return the shareDeposits
	 */
	public double getShareDeposits() {
		return shareDeposits;
	}

	/**
	 * @param shareDeposits the shareDeposits to set
	 */
	public void setShareDeposits(double shareDeposits) {
		this.shareDeposits = shareDeposits;
	}

	/**
	 * @param cashAmount the cashAmount to set
	 */
	public void setCashAmount(double cashAmount) {
		this.cashAmount = cashAmount;
	}

	/**
	 * @param depositAmount the depositAmount to set
	 */
	public void setDepositAmount(double depositAmount) {
		this.depositAmount = depositAmount;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.DepositDemander#getDepositAmount()
	 */
	@Override
	public double getDepositAmount() {
		return this.depositAmount;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.DepositDemander#getCashAmount()
	 */
	@Override
	public double getCashAmount() {
		return this.cashAmount;
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
	


	/**
	 * @return the dividendsReceived
	 */
	public double getDividendsReceived() {
		return dividendsReceived;
	}

	/**
	 * @param dividendsReceived the dividendsReceived to set
	 */
	public void setDividendsReceived(double dividendsReceived) {
		this.dividendsReceived = dividendsReceived;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.LaborSupplier#getPayableStock(int)
	 */
	@Override
	public Item getPayableStock(int idMarket) {
		switch(idMarket){
		case StaticValues.MKT_LABOR:
			return this.getItemStockMatrix(true, StaticValues.SM_DEP);
		case StaticValues.MKT_COCOBONDS:
			return this.getItemStockMatrix(true, StaticValues.SM_DEP);
		}
		return null;
	}

	@Override
	public void onAgentArrival(AgentArrivalEvent event) {
		MacroSimulation macroSim = (MacroSimulation)event.getSimulationController().getSimulation();
		int marketID=macroSim.getActiveMarket().getMarketId();
		switch(marketID){
		case StaticValues.MKT_CONSGOOD:
			SelectSellerStrategy sellerStrategy = (SelectSellerStrategy) this.getStrategy(StaticValues.STRATEGY_BUYING);
			MacroAgent seller = sellerStrategy.selectGoodSupplier(event.getObjects(), this.demand, true); 
			macroSim.getActiveMarket().commit(this, seller, marketID);
			break;
		case StaticValues.MKT_DEPOSIT:
			SelectDepositSupplierStrategy depStrategy = (SelectDepositSupplierStrategy)this.getStrategy(StaticValues.STRATEGY_DEPOSIT);
			MacroAgent depositSupplier = depStrategy.selectDepositSupplier(event.getObjects(), this.depositAmount);
			macroSim.getActiveMarket().commit(this, depositSupplier, marketID);
			break;
		case StaticValues.MKT_COCOBONDS:
			TwoStepMarketSimulation sim = (TwoStepMarketSimulation)macroSim.getActiveMarket();
			/*SelectCoCoBondSupplierStrategy cocoStrategy = (SelectCoCoBondSupplierStrategy)this.getStrategy(StaticValues.STRATEGY_COCOBOND);*/
			/*MacroAgent cocobondSupplier = sellerStrategy.selectCoCoBondSupplier(event.getObjects(), this.cocobondnominalDemand);*/
			/*MacroAgent cocobondSupplier = cocoStrategy.selectCoCoBondSupplier(event.getObjects(), cocobondDemand);*/
			if(sim.isFirstStep()){
				this.selectedCoCoBondSuppliers=event.getObjects();
				/*this.selectedCoCoBondSupplier=(CoCoBondSupplier) (cocobondSupplier);*/
				/*this.cocobondPrice=this.selectedCoCoBondSuppliers.getCoCoBondPrice();*/
				/*this.cocobondInterestRate=this.selectedCoCoBondSupplier.getCoCoBondInterestRate(this, cocobondnominalDemand);*/
				/*this.cocobondInterestRate=this.selectedCoCoBondSuppliers.getCoCoBondInterestRate();*/
			}else if(sim.isSecondStep()) {
				int nbSellers = this.selectedCoCoBondSuppliers.size();
				for(int i=0; i<nbSellers&&this.selectedCoCoBondSuppliers.size()>0;i++){
					SelectCoCoBondSupplierStrategy cocoStrategy = (SelectCoCoBondSupplierStrategy) this.getStrategy(StaticValues.STRATEGY_COCOBOND);
					MacroAgent cocobondSupplier = cocoStrategy.selectCoCoBondSupplier(this.selectedCoCoBondSuppliers, 0.0, true);
					macroSim.getActiveMarket().commit(this, cocobondSupplier, marketID);
					this.selectedCoCoBondSuppliers.remove(cocobondSupplier);
				}
			break;
			}
		}
	}
	

	/* (non-Javadoc)
	 * @see jmab.agents.SimpleAbstractAgent#onTicArrived(int)
	 */
	@Override
	protected void onTicArrived(MacroTicEvent event) {
		switch(event.getTic()){
		case StaticValues.TIC_COMPUTEEXPECTATIONS:
			this.computeExpectations();
			break;
		case StaticValues.TIC_LABORSUPPLY:
			computeWage();
			break;
		case StaticValues.TIC_CONSUMPTIONDEMAND:
			computeConsumptionDemand();
			break;
		case StaticValues.TIC_COCOBONDDEMAND:
			/*determineCoCoBondDemand();*/
			SelectCoCoBondSupplierStrategy cocoStrategy = (SelectCoCoBondSupplierStrategy) this.getStrategy(StaticValues.STRATEGY_COCOBOND);
			determineCoCoBondDemand(cocoStrategy.selectCoCoBondSupplier(this.selectedCoCoBondSuppliers, 0.0, true));
			break;
		case StaticValues.TIC_DEPOSITDEMAND:
			computeLiquidAssetsAmounts();
			break;
		case StaticValues.TIC_UPDATEEXPECTATIONS:
			updateExpectations();
			break;
		}
	}
	
	/**
	 * 
	 */
	private void computeWage() {
		WageStrategy strategy= (WageStrategy)this.getStrategy(StaticValues.STRATEGY_WAGE);
		this.wage=strategy.computeWage();
		if(this.employer==null){
			this.setActive(true, StaticValues.MKT_LABOR);
		}
	}

	/**
	 * 
	 */
	private void updateExpectations() {
		double[] price=new double[1];
		price[0]=0;
		List<Item> cons = this.getItemsStockMatrix(true, StaticValues.SM_CONSGOOD);
		int qty=0;
		for(Item item:cons){
			ConsumptionGood c = (ConsumptionGood)item;
			price[0]+=c.getPrice()*c.getQuantity();
			qty+=c.getQuantity();
		}
		if (qty!=0){
		price[0]=price[0]/qty;}
		else{
			price[0]=this.getExpectation(StaticValues.EXPECTATIONS_CONSPRICE).getExpectation();
		}
		this.getExpectation(StaticValues.EXPECTATIONS_CONSPRICE).addObservation(price);
		double nW=this.getNetWealth();
		this.addValue(StaticValues.LAG_NETWEALTH,nW);
		double employed;
		if(this.employer==null)
			employed=0;
		else
			employed=1;
		this.addValue(StaticValues.LAG_EMPLOYED,employed);
		List<Item> cocobond=this.getItemsStockMatrix(true, StaticValues.SM_COCOBONDS);
		for(Item ccb:cocobond) {
			CoCoBond cocob = (CoCoBond)ccb;
			double cocointerest=cocob.getInterestRate();
			double cocoheld=cocob.getValue(); /*somme des get value*/
			double cocointerestreceived=cocointerest*cocoheld;
		cocobondInterestReceived = cocointerestreceived;
		}
		this.cleanSM();
	}

	/**
	 * 
	 */
	private void computeLiquidAssetsAmounts() {
		double liquidAssets=0;
		List<Item> deposits=this.getItemsStockMatrix(true, StaticValues.SM_CASH);
		List<Item> cash=this.getItemsStockMatrix(true, StaticValues.SM_DEP);
		for (Item i: deposits){
			liquidAssets+=i.getValue();
		}
		for (Item i: cash){
			liquidAssets+=i.getValue();
		}
		this.setDepositAmount(this.shareDeposits*liquidAssets);
		this.setCashAmount(liquidAssets-this.getDepositAmount());	
		this.setActive(true, StaticValues.MKT_DEPOSIT);
	}

	
	/**
	 * Determine CoCo demand
	 */
	
	protected void determineCoCoBondDemand(MacroAgent selectedCoCoBondSupplier) {
		CoCoBondDemandStrategy strategy = (CoCoBondDemandStrategy)this.getStrategy(StaticValues.STRATEGY_COCOBONDDEMAND);
		/*this.cocobondDemand=strategy.cocobondDemand(this.selectedCoCoBondSuppliers, this);*/
		this.cocobondnominalDemand=strategy.cocobondnominalDemand(selectedCoCoBondSupplier);
		if(this.cocobondnominalDemand>0)
			this.setActive(true, StaticValues.MKT_COCOBONDS);
	}
	
	
	/**
	 * 
	 */
	private void computeConsumptionDemand() {
		this.updateIncome();
		ConsumptionStrategy strategy = (ConsumptionStrategy)this.getStrategy(StaticValues.STRATEGY_CONSUMPTION);
		this.setDemand(strategy.computeRealConsumptionDemand(), StaticValues.MKT_CONSGOOD);
		this.addValue(StaticValues.LAG_CONSUMPTION, this.demand);
		if (this.demand>0){
			this.setActive(true, StaticValues.MKT_CONSGOOD);
		}
	}

	
	/* (non-Javadoc)
	 * @see jmab.agents.GoodDemander#getPayingStocks(int, jmab.goods.Item)
	 */
/*	@Override
	public List<Item> getPayingStocks(int idItem, Item payableStock) {
		List<Item> result = new ArrayList<Item>();
		switch(idItem){
		case StaticValues.SM_CONSGOOD:
			if(payableStock instanceof Deposit){
				result.addAll(this.getItemsStockMatrix(true, StaticValues.SM_DEP));
				result.addAll(this.getItemsStockMatrix(true, StaticValues.SM_CASH));
			}else{
				result.addAll(this.getItemsStockMatrix(true, StaticValues.SM_CASH));
				result.addAll(this.getItemsStockMatrix(true, StaticValues.SM_DEP));			
			}
		case StaticValues.SM_COCOBONDS:
			result.addAll(this.getItemsStockMatrix(true, StaticValues.SM_DEP));
		}
		return result;
	}
*/	
	
	
	/* (non-Javadoc)
	 * @see jmab.agents.GoodDemander#getPayingStocks(int, jmab.goods.Item)
	 */
	@Override
	public List<Item> getPayingStocks(int idGood, Item payableStock) {
		List<Item> result = new ArrayList<Item>();
		if(payableStock instanceof Deposit){
			result.addAll(this.getItemsStockMatrix(true, StaticValues.SM_DEP));
			result.addAll(this.getItemsStockMatrix(true, StaticValues.SM_CASH));
		}else{
			result.addAll(this.getItemsStockMatrix(true, StaticValues.SM_CASH));
			result.addAll(this.getItemsStockMatrix(true, StaticValues.SM_DEP));			
		}
		return result;
	}
	
	@Override
	public List<Item> getPayingStocks1(int idCoCoBondSM, Item payableStock1) {
		List<Item> result1 = new ArrayList<Item>();
		if(payableStock1 instanceof Deposit){
			result1.addAll(this.getItemsStockMatrix(true, StaticValues.SM_DEP));
			result1.addAll(this.getItemsStockMatrix(true, StaticValues.SM_CASH));
		}else{
			result1.addAll(this.getItemsStockMatrix(true, StaticValues.SM_CASH));
			result1.addAll(this.getItemsStockMatrix(true, StaticValues.SM_DEP));
		}
		return result1;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.GoodDemander#getDemand()
	 */
	@Override
	public double getDemand(int idMarket) {
		return demand;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.GoodDemander#setDemand(double)
	 */
	@Override
	public void setDemand(double demand, int idMarket) {
		this.demand=demand;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.TaxPayer#payTaxes(jmab.goods.Item)
	 */
	@Override
	public void payTaxes(Item account) {
		TaxPayerStrategy strategy = (TaxPayerStrategy)this.getStrategy(StaticValues.STRATEGY_TAXES);
		double taxes=strategy.computeTaxes();
		Cash cash = (Cash)this.getItemStockMatrix(true, StaticValues.SM_CASH);
		double liquidity=cash.getValue();
		Deposit deposit= (Deposit)this.getItemStockMatrix(true, StaticValues.SM_DEP);
		liquidity+=deposit.getValue();
		taxes=Math.min(taxes, liquidity);
		double depValue=deposit.getValue();
		if(depValue<taxes){
			double cashTr=taxes-depValue;
			Item bCash=deposit.getLiabilityHolder().getItemStockMatrix(true, cash.getSMId());
			bCash.setValue(bCash.getValue()+cashTr);
			cash.setValue(cash.getValue()-cashTr);
			deposit.setValue(taxes);
		}
		this.addValue(StaticValues.LAG_TAXES, taxes);
		Item res = deposit.getLiabilityHolder().getItemStockMatrix(true,account.getSMId());
		res.setValue(res.getValue()-taxes);
		deposit.setValue(depValue-taxes);
		account.setValue(account.getValue()+taxes);
		double nW=this.getNetWealth();
		this.addValue(StaticValues.LAG_NETWEALTH, nW);
		this.dividendsReceived=0;
	}

	/**
	 * 
	 */
	private void updateIncome() {
		this.addValue(StaticValues.LAG_INCOME, this.getNetIncome());
	}

	/* (non-Javadoc)
	 * @see jmab.agents.AbstractHousehold#getIncome()
	 */
	@Override
	public double getNetIncome() {
			TaxPayerStrategy strategy = (TaxPayerStrategy)this.getStrategy(StaticValues.STRATEGY_TAXES);
			double taxes=strategy.computeTaxes();
			if(this.isEmployed()){
				double grossIncome = this.getGrossIncome();
				double netIncome= grossIncome-taxes;
				return netIncome;
			}
			else{
				MacroPopulation macroPop = (MacroPopulation) ((SimulationController)this.scheduler).getPopulation();
				Population households= (Population) macroPop.getPopulation(StaticValues.HOUSEHOLDS_ID);
				double averageWage=0;
				double employed=0;
				for(Agent agent:households.getAgents()){
					Households worker= (Households) agent;
					if (worker.getEmployer()!=null){
						averageWage+=worker.getWage();
						employed+=1;
					}
				}
				averageWage=averageWage/employed;
				GovernmentAntiCyclical gov = (GovernmentAntiCyclical)macroPop.getPopulation(StaticValues.GOVERNMENT_ID).getAgentList().get(0);
				double grossIncome = gov.getUnemploymentBenefit()*averageWage+this.getGrossIncome();
				double netIncome= grossIncome-taxes;
				return netIncome;
			}
		//return this.getPassedValue(StaticValues.LAG_INCOME, 1);	
	}

	/* (non-Javadoc)
	 * @see jmab.agents.WageSetterWithTargets#getWageLowerBound()
	 */
	@Override
	public double getWageLowerBound() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.WageSetterWithTargets#getWageUpperBound()
	 */
	@Override
	public double getWageUpperBound() {
		// TODO Auto-generated method stub
		return 0;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.WageSetterWithTargets#getReferenceVariableForWage()
	 */
	@Override
	public double getMicroReferenceVariableForWage() {
		if(this.employer==null){
			double averageUnemployment = 1;
			for(int i=1; i<this.employmentWageLag;i++){
				averageUnemployment+=this.getPassedValue(StaticValues.LAG_EMPLOYED, i);
			}
			return averageUnemployment/employmentWageLag;
		}
		else
			return 0;
	}

	/**
	 * @return the employmentWageLag
	 */
	public int getEmploymentWageLag() {
		return employmentWageLag;
	}

	/**
	 * @param employmentWageLag the employmentWageLag to set
	 */
	public void setEmploymentWageLag(int employmentWageLag) {
		this.employmentWageLag = employmentWageLag;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.WageSetterWithTargets#getMacroReferenceVariableForWage()
	 */
	@Override
	public double getMacroReferenceVariableForWage() {
		if(this.employer==null)
			return 0;
		else
			return this.getAggregateValue(StaticValues.LAG_AGGUNEMPLOYMENT, 1);
	}
	
	/**
	 * @return the employer
	 */
	public MacroAgent getEmployer() {
		return employer;
	}

	/**
	 * @param employer the employer to set
	 */
	public void setEmployer(MacroAgent employer) {
		this.employer = employer;
	}

	/* (non-Javadoc)
	 * @see jmab.agents.DepositDemander#interestPaid(double)
	 */
	@Override
	public void interestPaid(double interests) {
		this.interestsReceived=interests;
		
	}
	
	public double getInterestReceived(){
		return this.interestsReceived;
	}
	
	public int getcocobondnominalDemand(CoCoBondSupplier issuer) {
		return cocobondnominalDemand;
	}
	
	public int getcocobondnominalDemand() {
		return cocobondnominalDemand;
	}
	
	public void setCoCoBondNominalDemand(int cocobondnominalDemand) {
		this.cocobondnominalDemand = cocobondnominalDemand;
	}
	
	@Override
	public void setCoCoBondInterestsReceived(double cocoInterestReceived) {		
	}
	
	public double getCoCoBondInterestReceived(){
		return this.cocobondInterestReceived;
	}
	
	
	public double getcocoshare() {
		return cocoshare;
	}
	
	public void setcocoshare(double cocoshare) {
		this.cocoshare =cocoshare;
	}
	
	@Override
	public double getGrossIncome(){
		return super.getGrossIncome()+this.interestsReceived+this.dividendsReceived+this.cocobondInterestReceived;
	}
	

	
	/**
	 * Populates the agent characteristics using the byte array content. The structure is as follows:
	 * [sizeMacroAgentStructure][MacroAgentStructure][demand][cashAmount][depositAmount][shareDeposits][interestReceived][dividendsReceived]
	 * [employmentWageLag][matrixSize][stockMatrixStructure][expSize][ExpectationStructure][passedValSize][PassedValStructure]
	 * [stratsSize][StrategiesStructure]
	 */
	@Override
	public void populateAgent(byte[] content, MacroPopulation pop) {
		ByteBuffer buf = ByteBuffer.wrap(content);
		byte[] macroBytes = new byte[buf.getInt()];
		buf.get(macroBytes);
		super.populateCharacteristics(macroBytes, pop);
		demand = buf.getDouble();
		cashAmount = buf.getDouble();
		depositAmount = buf.getDouble();
		cocobondInterestRate = buf.getDouble();
		cocobondPrice = buf.getDouble();		
		shareDeposits = buf.getDouble();
		cocoshare = buf.getDouble();
		cocobondInterestReceived = buf.getDouble();
		cocobondnominalDemand = buf.getInt();
		/*cocobondDemand = buf.getInt();*/
		dividendsReceived = buf.getDouble();
		employmentWageLag = buf.getInt();
		
		
		/*A peut-être virer*/
		/*Collection<Agent> aHolders = pop.getPopulation(buf.getInt()).getAgents();
		long selSupplierId = buf.getLong(); 
		for(Agent a:aHolders){
			MacroAgent pot = (MacroAgent) a;
			if(pot.getAgentId()==selSupplierId){
				this.selectedCoCoBondSupplier=(CoCoBondSupplier)pot;
				break;
			}
		}*/
		
		int nbSuppliers = buf.getInt();
		this.selectedCoCoBondSuppliers = new ArrayList<Agent>();
		for(int i = 0 ; i < nbSuppliers ; i++){
			Collection<Agent> aHolders = pop.getPopulation(buf.getInt()).getAgents();
			long selSupplierId = buf.getLong(); 
			for(Agent a:aHolders){
				MacroAgent pot = (MacroAgent) a;
				if(pot.getAgentId()==selSupplierId){
					this.selectedCoCoBondSuppliers.add(pot);
				}
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
	 * Generates the byte array containing all relevant informations regarding the household agent. The structure is as follows:
	 * [sizeMacroAgentStructure][MacroAgentStructure][demand][cashAmount][depositAmount][shareDeposits][interestReceived][dividendsReceived]
	 * [employmentWageLag][matrixSize][stockMatrixStructure][expSize][ExpectationStructure][passedValSize][PassedValStructure]
	 * [stratsSize][StrategiesStructure]
	 */
	@Override
	public byte[] getBytes() {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		try {
			byte[] charBytes = super.getAgentCharacteristicsBytes();
			out.write(ByteBuffer.allocate(4).putInt(charBytes.length).array());
			out.write(charBytes);
			ByteBuffer buf = ByteBuffer.allocate(44);
			buf.putDouble(demand);
			buf.putDouble(cashAmount);
			buf.putDouble(depositAmount);
			buf.putDouble(cocobondInterestRate);
			buf.putDouble(cocobondPrice);
			buf.putDouble(shareDeposits);
			buf.putDouble(cocoshare);
			buf.putDouble(cocobondInterestReceived);
			/*buf.putInt(cocobondDemand);*/
			buf.putInt(cocobondnominalDemand);
			buf.putDouble(dividendsReceived);
			buf.putInt(employmentWageLag);
			buf.putInt(this.selectedCoCoBondSuppliers.size());
			for(Agent supplier:selectedCoCoBondSuppliers){
				buf.putInt(((MacroAgent)supplier).getPopulationId());
				buf.putLong(((MacroAgent)supplier).getAgentId());
			}
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
				case StaticValues.SM_COCOBONDS:
					it = new CoCoBond(itemData, pop, this);
					MacroAgent cocobondHolder = it.getLiabilityHolder();
					if(cocobondHolder.getAgentId()!=this.agentId)
						cocobondHolder.addItemStockMatrix(it, false, stockId);
					break;								
				case StaticValues.SM_CONSGOOD:
					it = new ConsumptionGood(itemData, pop, this);
					break;
				case StaticValues.SM_DEP:
					it = new Deposit(itemData, pop, this);
					MacroAgent depHolder = it.getLiabilityHolder();
					if(depHolder.getAgentId()!=this.agentId)
						depHolder.addItemStockMatrix(it, false, stockId);
					break;
				default:
					it = new Cash(itemData, pop, this);
					MacroAgent cashHolder = it.getLiabilityHolder();
					if(cashHolder.getAgentId()!=this.agentId)
						cashHolder.addItemStockMatrix(it, false, stockId);
					break;
				}
				this.addItemStockMatrix(it, true, stockId);
				
			}
		}	
		
	}

	@Override
	public void setLaborActive(boolean active) {
		this.setActive(active,StaticValues.MKT_LABOR);
		
	}

}
