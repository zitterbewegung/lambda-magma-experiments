/**
 * Author: Joshua Jay Herman Advisor: Louis Kauffman
 * Copyright 2012 University of Illinois At Chicago
 * Created 1:16:58 PM
 */
package edu.uic.math;

/**
 * @author zitterbewegung
 *
 */
public class MagmaVar {
	
	String varname;

	MagmaVar(String varname){
		
		this.varname = varname;
	}
	
	public boolean equals(Object o){
		if (!(o instanceof MagmaVar))
            return false;
		MagmaVar p = (MagmaVar)o;
		
		return (p.isZ() == this.isZ() && p.varname == this.varname);
		
	}
	public boolean isZ(){
		return this.varname == "Z";
		
	}
	
	public void test(){
		MagmaVar test1 = null;
		MagmaVar test2 = new MagmaVar("test");
		MagmaVar test3 = new MagmaVar("Z");
		assert test1 == null;
		assert test2.isZ() == false; 
		assert test3.isZ() == true;
	}
}
