
package jmab.strategies;

import java.util.ArrayList;
import java.util.List;

import jmab.agents.MacroAgent;
import net.sourceforge.jabm.agent.Agent;

/**
 * @author Elise Kremer
 *
 */
public interface SelectCoCoBondSupplierStrategy extends SingleStrategy {

	public MacroAgent selectCoCoBondSupplier(ArrayList<Agent> suppliers, double amount, boolean real);
	
	public List<MacroAgent> selectMultipleCoCoBondSupplier(ArrayList<Agent> suppliers,double amount, boolean real);

	
}
