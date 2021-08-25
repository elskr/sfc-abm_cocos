package benchmark.strategies;

import jmab.population.MacroPopulation;
import jmab.strategies.CoCoBondDemandStrategy;
import net.sourceforge.jabm.Population;
import net.sourceforge.jabm.SimulationController;
import net.sourceforge.jabm.agent.Agent;
import net.sourceforge.jabm.strategy.AbstractStrategy;
import jmab.agents.MacroAgent;

import java.nio.ByteBuffer;

import benchmark.StaticValues;
import benchmark.agents.Households;
import benchmark.agents.Bank;


/**
 * @author Elise Kremer
 *
 */
@SuppressWarnings("serial")
public class CoCoBondDemandArbitrage extends AbstractStrategy implements CoCoBondDemandStrategy{
	
	protected double paramArbitrage1;
	protected double paramArbitrage2;
	protected double share;



	public int cocobondnominalDemand(MacroAgent supplier) {
		Households household = (Households) getAgent();
		SimulationController controller = (SimulationController)this.getScheduler();
		MacroPopulation macroPop = (MacroPopulation) controller.getPopulation();
		Population banks = macroPop.getPopulation(StaticValues.BANKS_ID);
		
		double tot=0;
		double inter=0;
		double pastissuedcocos = 0;
		double pastinterestpaid = 0;
		for (Agent b:banks.getAgents()){
			Bank bank1 = (Bank) b;
			tot+=bank1.getPassedValue(StaticValues.LAG_COCOTRIGGERED, 1);
			inter+=bank1.getPassedValue(StaticValues.LAG_DEPOSITINTEREST, 1);
			pastissuedcocos = bank1.getPassedValue(StaticValues.LAG_COCOISSUED,1);
			pastinterestpaid = bank1.getPassedValue(StaticValues.LAG_COCOINTERESTSPAID,1);
			}
		double probability =tot/banks.getSize();
		double avrgdepinterest=inter/banks.getSize();
		double avrgpastissuedcocos=pastissuedcocos/banks.getSize();
		double avrgpastinterestpaid=pastinterestpaid/banks.getSize();
		/*double div = bank.getPassedValue(StaticValues.LAG_DIVIDENDS, 1);*/
		/*double operatingCashFlow=bank.getPassedValue(StaticValues.LAG_PROFITAFTERTAX, 1);*/
		/*double householdRiskAversion= household.getRiskAversion();*/
		/*double probability;
		if (div != 0)
			probability=1/(1+Math.exp((operatingCashFlow-householdRiskAversion*div)/div));
		else
			probability = 0;*/
		double shareCoCos;
		if ((avrgpastinterestpaid / avrgpastissuedcocos) >= avrgdepinterest && avrgpastissuedcocos > 0) 
			shareCoCos = paramArbitrage1*Math.exp(-(paramArbitrage2)*(((avrgpastinterestpaid/avrgpastissuedcocos))*(1-probability)-avrgdepinterest));
		else
			shareCoCos = paramArbitrage1;	
		
		share = shareCoCos;
		
		household.setcocoshare(1-share);
		
		/*double cocobondsPrice= supplier.getCoCoBondPrice();*/
		return (int) (Math.max(0, household.getNetWealth()*(1-shareCoCos)));
		/*return (int) Math.rint(Math.max(0, household.getItemStockMatrix(true, StaticValues.SM_DEP).getValue()*(1-shareCoCos)));*/
	}

	public double getParamArbitrage1() {
		return this.paramArbitrage1;
	}

	public void setParamArbitrage1(double paramArbitrage1) {
		this.paramArbitrage1 = paramArbitrage1;
	}
	
	public double getParamArbitrage2() {
		return this.paramArbitrage2;
	}

	public void setParamArbitrage2(double paramArbitrage2) {
		this.paramArbitrage2 = paramArbitrage2;
	}

	
	/**
	 * Generate the byte array structure of the strategy. The structure is as follow:
	 * [liquidityRatio]
	 * @return the byte array content
	 */
	@Override
	public byte[] getBytes() {
		ByteBuffer buf = ByteBuffer.allocate(8);
		buf.putDouble(this.paramArbitrage1);
		buf.putDouble(this.paramArbitrage2);
		buf.putDouble(this.share);
		return buf.array();
	}

	/**
	 * Populates the strategy from the byte array content. The structure should be as follows:
	 * [liquidityRatio]
	 * @param content the byte array containing the structure of the strategy
	 * @param pop the Macro Population of agents
	 */
	
	@Override
	public void populateFromBytes(byte[] content, MacroPopulation pop) {
		ByteBuffer buf = ByteBuffer.wrap(content);
		this.paramArbitrage1 = buf.getDouble();
		this.paramArbitrage2 = buf.getDouble();
		this.share = buf.getDouble();

	}
	
}
