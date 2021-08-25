
package jmab.mechanisms;

import java.util.List;

import jmab.agents.CoCoBondDemander;
import jmab.agents.CoCoBondSupplier;
import jmab.agents.MacroAgent;
import jmab.stockmatrix.CoCoBond;
import jmab.stockmatrix.Item;


/**
 * @author Elise Kremer
 *
 */
public class CoCoBondMechanism extends CopyOfAbstractMechanism implements Mechanism {
	
	private int idCoCoBondSM;
	private int idAdvancesSM;

	
	

	/**
	 * @return the idCoCoBondSM
	 */
	public int getIdCoCoBondSM() {
		return idCoCoBondSM;
	}

	/**
	 * @param idCoCoBondSM the idCoCoBondSM to set
	 */
	public void setIdCoCoBondSM(int idCoCoBondSM) {
		this.idCoCoBondSM = idCoCoBondSM;
	}
	
	public int getIdAdvancesSM() {
		return idAdvancesSM;
	}

	/**
	 * @param idAdvancesSM the idAdvancesSM to set
	 */
	public void setIdAdvancesSM(int idAdvancesSM) {
		this.idAdvancesSM = idAdvancesSM;
	}

	/* (non-Javadoc)
	 * @see jmab.mechanisms.Mechanism#execute(jmab.agents.MacroAgent, jmab.agents.MacroAgent, int)
	 */
	@Override
	public void execute(MacroAgent buyer, MacroAgent seller, int idMarket) {
		execute((CoCoBondDemander)buyer, (CoCoBondSupplier) seller, idMarket);
	}
	
	private void execute(CoCoBondDemander buyer, CoCoBondSupplier issuer, int idMarket){
		//0. Check first if there are CoCobonds to be sold
		CoCoBond cocobondsIssued = (CoCoBond) issuer.getItemStockMatrix(false, idCoCoBondSM,issuer); 
		if (cocobondsIssued!=null){
			//1. Determine quantity, price and total costs
			double price=cocobondsIssued.getPrice();
			double interestRate=cocobondsIssued.getInterestRate();
			int maturity=cocobondsIssued.getMaturity();
			double cocobondsDemanded = 0;
			if (price > 0)
				cocobondsDemanded = buyer.getcocobondnominalDemand(issuer)/price;
			else 
				cocobondsDemanded = 0;
			double quantity=Math.min(cocobondsDemanded, (int) cocobondsIssued.getQuantity());
			if (cocobondsDemanded==quantity){
				buyer.setActive(false, idMarket);
			}
			double totalAmount=quantity*price;
			//2. Prepare the re-allocation of funds
			//2.1 Get the payable stock
			Item payableStock1 = issuer.getPayableStock1(idCoCoBondSM);
			//2.2 Get the paying stocks
			List<Item> payingStocks1 = buyer.getPayingStocks1(idCoCoBondSM,payableStock1);
			//2.3 Get the first occurrence of an item of the same sort than the payable stock within the paying stocks
			Item targetStock1=null;
			for(Item item:payingStocks1){
				targetStock1=item;
				break;
			}
			//4. If not enough money was raised
			if (targetStock1.getValue()<totalAmount){
				//Then the good demander is deactivated and quantities and total costs are updated accordingly
				quantity= (int) Math.floor(targetStock1.getValue()/price);
				totalAmount=quantity*price;
				buyer.setActive(false, idMarket);
			}
			if(quantity>0){
				//5. Do the transfer from targetStock towards payable stock
				/*{LiabilitySupplier payingSupplier = (LiabilitySupplier) targetStock1.getLiabilityHolder();*/
				/*Item balancingItem = issuer.getItemStockMatrix(true, idAdvancesSM);*/
				targetStock1.setValue(targetStock1.getValue()-totalAmount);
				payableStock1.setValue(payableStock1.getValue()+totalAmount);
				/*payingSupplier.transfer(targetStock1, payableStock1, totalAmount);*/
				//6. Do the transfer of the bonds
				CoCoBond cocobondsPurchased = new CoCoBond(price*quantity, (double)quantity, buyer, issuer, maturity, interestRate, price);
				cocobondsIssued.setQuantity(cocobondsIssued.getQuantity()-quantity);
				buyer.addItemStockMatrix(cocobondsPurchased, true, idCoCoBondSM);
				issuer.addItemStockMatrix(cocobondsPurchased, false, idCoCoBondSM);
				//7. If there are no more cocobonds to be sold, then the supplier is deactivated.
				if (cocobondsIssued.getQuantity()==0){
					issuer.removeItemStockMatrix(cocobondsIssued, false, idCoCoBondSM);
					issuer.setActive(false, idMarket);
				}
			}
		}
		
	}
	
	/* (non-Javadoc)
	 * @see jmab.mechanisms.Mechanism#execute(jmab.agents.MacroAgent, java.util.List, int)
	 */
	@Override
	public void execute(MacroAgent buyer, List<MacroAgent> seller, int idMarket) {
		// TODO Auto-generated method stub
		
	}

}
