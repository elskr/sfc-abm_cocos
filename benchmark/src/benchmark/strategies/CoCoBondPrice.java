package benchmark.strategies;

import java.nio.ByteBuffer;
import java.util.List;

import benchmark.StaticValues;
import benchmark.agents.Bank;
import benchmark.agents.Households;
import jmab.population.MacroPopulation;
import jmab.stockmatrix.CoCoBond;
import jmab.stockmatrix.Item;
import jmab.strategies.CoCoBondPricingStrategy;
import net.sourceforge.jabm.Population;
import net.sourceforge.jabm.SimulationController;
import net.sourceforge.jabm.agent.Agent;
import net.sourceforge.jabm.strategy.AbstractStrategy;

/**
 * @author Elise Kremer
 *
 */
@SuppressWarnings("serial")
public class CoCoBondPrice extends AbstractStrategy implements
		CoCoBondPricingStrategy {
	
	
	private int mktId;	

	/*private double paramtrigger;*/
	
	public double computeCoCoBondPrice() {
		SimulationController controller = (SimulationController)this.getScheduler();
		MacroPopulation macroPop = (MacroPopulation) controller.getPopulation();
		Population banks = macroPop.getPopulation(StaticValues.BANKS_ID);
		Population households = macroPop.getPopulation(StaticValues.HOUSEHOLDS_ID);

		double aggdemand = 0;
		double cocos = 0;
			
		for (Agent b:banks.getAgents()){
			Bank bank = (Bank) b;
			List<Item>loans=bank.getItemsStockMatrix(false, StaticValues.SM_COCOBONDS);
			for (Item j:loans){
				CoCoBond coco= (CoCoBond)j;
				cocos+=coco.getQuantity();
				}
		}
		
		for (Agent hh:households.getAgents()){
			Households household =(Households) hh;
			aggdemand+=household.getcocobondnominalDemand();
		}
		if (cocos > 0)
			return aggdemand/cocos;
		else
			return 0;
	}
		
		/*CoCoBondSupplier seller=(CoCoBondSupplier) this.getAgent();
		double capitalsValue=seller.getNetWealth();
		double sharecocos=seller.getTargetedAdditionalTier1();
		double needcocos;
		double pastcocobondPrice = seller.getPassedValue(StaticValues.LAG_COCOBONDSPRICE,1);
		
		if (seller.getLiquidityRatio() >= seller.getParamTrigger()) 
			needcocos = capitalsValue*sharecocos;
		else
			needcocos = 0;	
		int quantity = 0;
		if(needcocos>0){
			quantity = (int)Math.ceil(needcocos/pastcocobondPrice);
		}
		if (quantity != 0)
			return buyer.getcocobondnominalDemand(seller)/quantity;
		else
			return 0;
	}*/
		
	/**
	 * @return the mktId
	 */
	public int getMktId() {
		return mktId;
	}

	/**
	 * @param mktId the mktId to set
	 */
	public void setMktId(int mktId) {
		this.mktId = mktId;
	}
	
	@Override
	public byte[] getBytes() {
		ByteBuffer buf = ByteBuffer.allocate(28);
		/*buf.putDouble(this.paramtrigger);*/
		buf.putInt(this.mktId);
		return buf.array();
	}
	
	@Override
	public void populateFromBytes(byte[] content, MacroPopulation pop) {
		ByteBuffer buf = ByteBuffer.wrap(content);
		/*this.paramtrigger = buf.getDouble();*/
		this.mktId = buf.getInt();

	}

}