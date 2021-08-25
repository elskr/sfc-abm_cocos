package jmab.agents;

import jmab.stockmatrix.Item;

/**
 * @author Elise Kremer
 *
 */
public interface CoCoBondSupplier extends MacroAgent {
	
	public Item getPayableStock1(int idCoCoBondSM);
	
	public double getCoCoBondPrice();
	
	public double getCoCoBondInterestRate(MacroAgent cocobondDemander, double amount);
	
	public double getCoCoBondInterestRate();
	
	public int getCoCoBondSupply();
	
	public double getTargetedAdditionalTier1();
	
	public double getLiquidityRatio();
	
	public double getParamTrigger();
	
	public double getTotInterestsCoCoBonds();
}
