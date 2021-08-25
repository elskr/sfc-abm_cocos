package jmab.stockmatrix;

import java.nio.ByteBuffer;

import jmab.agents.MacroAgent;
import jmab.population.MacroPopulation;

/**
 * @author Elise Kremer
 *
 * Class that represents a Contingent Convertible bond.
 */
public class CoCoBond extends AbstractItem implements Item, InterestBearingItem {

	private int maturity;
	private double interestRate;
	private double price;
	
	/**
	 * 
	 */
	public CoCoBond() {
	}

	/**
	 * @param value the value of the CoCo bond
	 * @param quantity the quantity of CoCo bonds
	 * @param assetHolder the holder of the CoCo bond
	 * @param liabilityHolder the issuer of the CoCo bond
	 * @param maturity the maturity of the CoCo bond
	 * @param interestRate the interest rate of the CoCo bond
	 * @param price the price of the CoCo bond
	 */
	public CoCoBond(double value, double quantity, MacroAgent assetHolder,
			MacroAgent liabilityHolder, int maturity, double interestRate, double price) {
		super(value, quantity, assetHolder, liabilityHolder);
		this.interestRate=interestRate;
		this.maturity=maturity;
		this.price=price;
		this.age=0;
	}
	
	/**
	 * Creates the item from an array of bytes, the structure for the bytes is the following (Super Structure + 20bytes in total):
	 * [SuperStructure][maturity][price][interestRate]
	 * @param content
	 * @param population
	 */
	public CoCoBond(byte[] content, MacroPopulation population, MacroAgent aHolder){
		super(content, population, aHolder);
		ByteBuffer reader = ByteBuffer.wrap(content);
		reader.position(content.length-24);
		this.maturity = reader.getInt();
		this.price = reader.getDouble();
		this.interestRate = reader.getDouble();
	}

	@Override
	public double getValue(){
		if(this.liabilityHolder==this.assetHolder)
			return 0;
		else
			return this.quantity*this.price;
	}
	
	/**
	 * @return the maturity
	 */
	public int getMaturity() {
		return maturity;
	}

	/**
	 * @param maturity the maturity to set
	 */
	public void setMaturity(int maturity) {
		this.maturity = maturity;
	}

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

	/**
	 * @return the price
	 */
	public double getPrice() {
		return price;
	}

	/**
	 * @param price the price to set
	 */
	public void setPrice(double price) {
		this.price = price;
	}

	/* (non-Javadoc)
	 * @see jmab.goods.AbstractItem#remove()
	 */
	@Override
	public boolean remove() {
		return maturity<age;
	}

	/* (non-Javadoc)
	 * @see jmab.goods.AbstractItem#update()
	 */
	@Override
	public void update() {
		age+=1;
	}

	/**
	 * generates the byte array representation of the item. The structure of the array is the following (Super structure + 20 bytes in total):
	 * [SuperStructure][maturity][price][interestRate]
	 * @return the byte array containing the relevant information of the item
	 */
	@Override
	public byte[] getBytes(){
		byte[] prevResult = super.getBytes();
		byte[] result = new byte[prevResult.length+24];
		ByteBuffer buffer = ByteBuffer.wrap(result).put(prevResult);
		buffer.putInt(this.maturity);
		buffer.putDouble(this.price);
		buffer.putDouble(this.interestRate);
		return result;
	}
}
