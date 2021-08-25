
package benchmark.strategies;

import java.nio.ByteBuffer;

import jmab.agents.MacroAgent;
import jmab.population.MacroPopulation;
import jmab.strategies.InterestRateStrategy;
import net.sourceforge.jabm.strategy.AbstractStrategy;

/**
 * @author Elise kremer
 *
 */
@SuppressWarnings("serial")
public class CoCoBondInterestRateStrategy extends AbstractStrategy implements
		InterestRateStrategy {

	private double interestRate;
	
	/**
	 * @return the interestRate
	 */
	public double getInterestRate() {
		return interestRate;
	}


	/**
	 * @param interestRate the interestRate to set
	 */
	public void setInterestRate(double interestRate) {
		this.interestRate = interestRate;
	}


	/* (non-Javadoc)
	 * @see jmab.strategies.InterestRateStrategy#computeInterestRate(jmab.agents.MacroAgent, double, int)
	 */
	@Override
	public double computeInterestRate(MacroAgent creditDemander, double amount, int length) {
		return this.interestRate;
	}
	
	/**
	 * Generate the byte array structure of the strategy. The structure is as follow:
	 * [interestRate]
	 * @return the byte array content
	 */
	@Override
	public byte[] getBytes() {
		ByteBuffer buf = ByteBuffer.allocate(8);
		buf.putDouble(this.interestRate);
		return buf.array();
	}


	/**
	 * Populates the strategy from the byte array content. The structure should be as follows:
	 * [interestRate]
	 * @param content the byte array containing the structure of the strategy
	 * @param pop the Macro Population of agents
	 */
	@Override
	public void populateFromBytes(byte[] content, MacroPopulation pop) {
		ByteBuffer buf = ByteBuffer.wrap(content);
		this.interestRate = buf.getDouble();
	}

}
