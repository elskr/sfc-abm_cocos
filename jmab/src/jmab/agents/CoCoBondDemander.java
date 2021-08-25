package jmab.agents;

import java.util.List;

import jmab.stockmatrix.Item;

/**
 * @author Elise Kremer
 *
 */
public interface CoCoBondDemander extends MacroAgent {

	/**
	 * @param issuer 
	 * @return
	 */
	/*int getCoCoBondsDemand(double price, CoCoBondSupplier issuer);*/
	int getcocobondnominalDemand (CoCoBondSupplier issuer);

	/**
	 * @param idCoCoBondSM
	 * @param payableStock
	 * @return
	 */
	List<Item> getPayingStocks1(int idCoCoBondSM, Item payableStock);
	
	public void setCoCoBondInterestsReceived(double interests);
	
}
