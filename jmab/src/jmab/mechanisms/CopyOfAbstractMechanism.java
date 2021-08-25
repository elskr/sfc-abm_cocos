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
package jmab.mechanisms;

import java.util.List;

import jmab.agents.LiabilitySupplier;
import jmab.simulations.MarketSimulation;
import jmab.stockmatrix.Item;

/**
 * @author Elise Kremer
 *
 */
public abstract class CopyOfAbstractMechanism implements Mechanism {

	protected MarketSimulation market;
	
	/**
	 * 
	 */
	public CopyOfAbstractMechanism() {
		super();
	}

	/**
	 * @param scheduler
	 * @param market
	 */
	public CopyOfAbstractMechanism(MarketSimulation market) {
		super();
		this.market = market;
	}

	/**
	 * @param market the market to set
	 */
	public void setMarket(MarketSimulation market) {
		this.market = market;
	}

	/* (non-Javadoc)
	 * @see jmab.mechanisms.Mechanism#setMarketSimulation(jmab.simulations.MarketSimulation)
	 */
	@Override
	public void setMarketSimulation(MarketSimulation market) {
		this.market=market;

	}

	/* (non-Javadoc)
	 * @see jmab.mechanisms.Mechanism#getMarketSimulation()
	 */
	@Override
	public MarketSimulation getMarketSimulation() {
		return market;
	}
	
	/**
	 * Re-allocates liquidity among different items of a stock matrix. Different case might arise
	 * a. Withdraw or deposit cash on deposit account
	 * b. Transfer from a long term deposit to a short term deposit
	 * c. Transfer from a deposit account to an other deposit account
	 * @param amount the amount to be raised from the different payingStocks
	 * @param payingStocks the list of items to be used to raise the amount
	 * @param targetStock the stock target for the amount to be raised
	 */
	public void reallocateLiquidity(double amount1, List<Item> payingStocks1, Item targetStock1){
		//The amount raised is equal to what is already on the target stock
		double amountRaised=targetStock1.getValue();
		for(int i=0;i<payingStocks1.size()&&amountRaised<amount1;i++){
			//For each item in the list
			Item payingStock1 = payingStocks1.get(i);
			//If the payingStock is not the target stock (otherwise, there's nothing to do).
			if(payingStock1!=targetStock1){
				//compute different amounts
				double thisAmount=payingStock1.getValue();
				double remAmount=Math.max(0,thisAmount+amountRaised-amount1);
				double transferAmount=thisAmount-remAmount;
				amountRaised+=transferAmount;
				//who is the supplier of the paying stock?
				LiabilitySupplier payingSupplier = (LiabilitySupplier) payingStock1.getLiabilityHolder();
				// Case c. If the paying stock and the target stock are of the same type, then ask the liability supplier 
				// to do a transfer
				if(payingStock1.getSMId()==targetStock1.getSMId()&&payingStock1!=targetStock1){
					payingSupplier.transfer(payingStock1, targetStock1, transferAmount);
				}else{
					targetStock1.setValue(targetStock1.getValue()+transferAmount);
					payingStock1.setValue(payingStock1.getValue()-transferAmount);
					// Check whether we are the situation of a triangle (that is only one balancing stock, typically 
					// the case of case a) or if the situation is a rectangle (2 different balancing stocks, case b).
					LiabilitySupplier targetSupplier = (LiabilitySupplier) targetStock1.getLiabilityHolder();
					Item balancingStock=payingSupplier.getCounterpartItem(payingStock1, targetStock1);
					Item otherBalancingStock=targetSupplier.getCounterpartItem(targetStock1, payingStock1);
					if(balancingStock==otherBalancingStock){
						//If triangle (case a), update only one stock (+ or - depending on asset or liability)
						if(balancingStock.getAssetHolder()==payingSupplier)
							balancingStock.setValue(balancingStock.getValue()-transferAmount);
						else
							balancingStock.setValue(balancingStock.getValue()+transferAmount);
					}else{
						//If rectangle (case b), update two stocks (+ or - depending on asset or liability)
						if(balancingStock.getAssetHolder()==payingSupplier){
							balancingStock.setValue(balancingStock.getValue()-transferAmount);
							otherBalancingStock.setValue(otherBalancingStock.getValue()+transferAmount);
						}else{
							balancingStock.setValue(balancingStock.getValue()+transferAmount);
							otherBalancingStock.setValue(otherBalancingStock.getValue()-transferAmount);
						}
					}
				}
			}
		}
	}
}