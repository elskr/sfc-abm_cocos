package jmab.strategies;

import jmab.agents.MacroAgent;

/**
 * @author Elise Kremer
 */
public interface CoCoBondDemandStrategy extends SingleStrategy {
	
	/*public int cocobondDemand(CoCoBondSupplier supplier, CoCoBondDemander demander);*/
	/*public int cocobondnominalDemand(CoCoBondSupplier supplier);*/
	public int cocobondnominalDemand(MacroAgent selectedCoCoBondSupplier);


}
