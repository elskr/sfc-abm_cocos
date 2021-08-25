
package jmab.strategies;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeMap;

import jmab.agents.CoCoBondDemander;
import jmab.agents.CoCoBondSupplier;
import jmab.agents.MacroAgent;
import jmab.population.MacroPopulation;
import net.sourceforge.jabm.agent.Agent;
import net.sourceforge.jabm.strategy.AbstractStrategy;

/**
 * @author Elise Kremer
 *
 */
@SuppressWarnings("serial")
public class MostPayingCoCoBondsWithSwitching extends AbstractStrategy implements
		SelectCoCoBondSupplierStrategy {

	private CoCoBondSupplier previousCoCoBondSupplier;
	private SwitchingStrategy strategy;
	
	/**
	 * @return the previousCoCoBondSupplier
	 */
	public CoCoBondSupplier getPreviousCoCoBondSupplier() {
		return previousCoCoBondSupplier;
	}

	/**
	 * @param previousCoCoBondSupplier the previousCoCoBondSupplier to set
	 */
	public void setPreviousCoCoBondSupplier(CoCoBondSupplier previousCoCoBondSupplier) {
		this.previousCoCoBondSupplier = previousCoCoBondSupplier;
	}

	/**
	 * @return the strategy
	 */
	public SwitchingStrategy getStrategy() {
		return strategy;
	}

	/**
	 * @param strategy the strategy to set
	 */
	public void setStrategy(SwitchingStrategy strategy) {
		this.strategy = strategy;
	}

	/* (non-Javadoc)
	 * @see jmab.strategies.SelectCoCoBondSupplierStrategy#selectCoCoBondSupplier(java.util.ArrayList, double)
	 */
	
	/* Maybe try to call AbstractBank instead of CoCoBondSupplier */
	@Override
	public MacroAgent selectCoCoBondSupplier(ArrayList<Agent> suppliers, double amount, boolean real) {
		double maxIR=Double.NEGATIVE_INFINITY;
		CoCoBondSupplier maxCoCoBondSupplier=(CoCoBondSupplier) suppliers.get(0);
		CoCoBondDemander cocobondDemander = (CoCoBondDemander) getAgent();
		for(Agent agent : suppliers){
			CoCoBondSupplier cocoSupplier=(CoCoBondSupplier)agent;
			double tempIR=cocoSupplier.getCoCoBondInterestRate(cocobondDemander, amount);
			if(tempIR>maxIR){
				maxIR=tempIR;
				maxCoCoBondSupplier=cocoSupplier;
			}
		}
		
		double previousIR=0;
		if (!previousCoCoBondSupplier.isDead()){
			/*previousIR=previousCoCoBondSupplier.getCoCoBondInterestRate(cocobondDemander);*/
			previousIR=previousCoCoBondSupplier.getCoCoBondInterestRate(cocobondDemander, amount);

		}
		else{
			previousIR=Double.NEGATIVE_INFINITY;
		}
		
		if(previousIR<maxIR){
			if(previousIR==Double.NEGATIVE_INFINITY||strategy.switches(previousIR,maxIR)){
				previousCoCoBondSupplier=maxCoCoBondSupplier;
				//System.out.println("switching");
			}
		}
		return previousCoCoBondSupplier;
	}
	
	
	public List<MacroAgent> selectMultipleCoCoBondSupplier(ArrayList<Agent> suppliers, double amount, boolean real) {
		TreeMap<Double,ArrayList<MacroAgent>> orederedSellers = new TreeMap<Double,ArrayList<MacroAgent>>();
		CoCoBondDemander buyer=(CoCoBondDemander)this.getAgent();
		for (Agent agent:suppliers){
			CoCoBondSupplier seller=(CoCoBondSupplier)agent;
			double interest=seller.getCoCoBondInterestRate(buyer, amount);
			if(orederedSellers.containsKey(interest)){
				ArrayList<MacroAgent> list = orederedSellers.get(interest);
				list.add(seller);
			}else{
				ArrayList<MacroAgent> list = new ArrayList<MacroAgent>();
				list.add(seller);
				orederedSellers.put(interest, list);
			}
		}
		ArrayList<MacroAgent> result = new ArrayList<MacroAgent>();
		for (Double key:orederedSellers.descendingKeySet()){
			for(MacroAgent agent:orederedSellers.get(key)){
				double previousInterest=previousCoCoBondSupplier.getCoCoBondInterestRate(buyer, amount);
				CoCoBondSupplier seller=(CoCoBondSupplier)agent;
				double interest=seller.getCoCoBondInterestRate(buyer, amount);
				if(previousInterest>interest){
					if(previousInterest==Double.POSITIVE_INFINITY||strategy.switches(previousInterest,interest)){
						result.add(agent);
					}else{
						result.add(previousCoCoBondSupplier);
					}
				}
			}
		}
		return result;
	}

	/**
	 * Generate the byte array structure of the strategy. The structure is as follow:
	 * [popId][previousCoCoBondSupplierId]
	 * @return the byte array content
	 */
	@Override
	public byte[] getBytes() {
		ByteBuffer buf = ByteBuffer.allocate(21);
		buf.putInt(this.previousCoCoBondSupplier.getPopulationId());
		buf.putLong(this.previousCoCoBondSupplier.getAgentId());
		return buf.array();
	}
	
	/**
	 * Populates the strategy from the byte array content. The structure should be as follows:
	 * [popId][previousCoCoBondSupplierId]
	 * @param content the byte array containing the structure of the strategy
	 * @param pop the Macro Population of agents
	 */
	@Override
	public void populateFromBytes(byte[] content, MacroPopulation pop) {
		ByteBuffer buf = ByteBuffer.wrap(content);
		Collection<Agent> aHolders = pop.getPopulation(buf.getInt()).getAgents();
		long cocobondSupplierId = buf.getLong(); 
		for(Agent a:aHolders){
			MacroAgent pot = (MacroAgent) a;
			if(pot.getAgentId()==cocobondSupplierId){
				this.previousCoCoBondSupplier=(CoCoBondSupplier) pot;
				break;
			}
		}
		
	}


	
}
