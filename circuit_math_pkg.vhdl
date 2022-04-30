library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package circuit_math_pkg is
	type fixed_values_t is record
		-- fixed values:
		V_V1: real;
		R_R1: real;
		R_R2: real;
		C_C1: real;
		L_L1: real;
	end record;
	
	type circuit_state_t is record
		timestamp: time;
		fixed: fixed_values_t;
		-- initial values:
		init_V_C1: real;
		init_I_L1: real;
		-- time dependent values:
		I_V1: real;
		V_R1: real;
		I_R1: real;
		V_R2: real;
		I_R2: real;
		V_C1: real;
		I_C1: real;
		V_L1: real;
		I_L1: real;
	end record;

	function circuit_evaluated(state: circuit_state_t; projected_time: time) return circuit_state_t;
end;

package body circuit_math_pkg is
	function circuit_evaluated(state: circuit_state_t; projected_time: time) return circuit_state_t is
		variable rval: circuit_state_t; -- Return value
		variable dt_t: time; -- Time from the start of the period.
		variable dt_r: real; -- Time from the start of the period (as real number).
		variable t: real; -- used in Maxima generated formulas.
	begin
		dt_t := projected_time - state.timestamp;
		dt_r := real(dt_t/1 ps) * 1.0e-12;
		t := dt_r;
		
		rval := state;
		
		rval.timestamp := projected_time;
		
		-- Using Notepad++ Replace with Regular expression to change from Maxima format to VHDL format:
		-- Find what: [A-Z]_[A-Z0-9]*
		-- Replace with: state.$0
		--
		-- plus a lot of manual editing...
		
		rval.I_V1 := 
			(exp(-((state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*t)/(2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2))*(((2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*(state.fixed.L_L1*state.fixed.R_R1*state.fixed.V_V1-state.init_I_L1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2)-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*(state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1))*sin((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2)))/(sqrt(state.fixed.L_L1)*sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0))+state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*cos((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2))))/(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2)-state.fixed.V_V1/state.fixed.R_R2;
		
		rval.V_R1 :=
			(exp(-((state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*t)/(2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2))*(((2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*(state.fixed.L_L1*state.fixed.R_R1*state.fixed.V_V1-state.init_I_L1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2)-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*(state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1))*sin((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2)))/(sqrt(state.fixed.L_L1)*sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0))+state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*cos((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2))))/(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2);
		
		rval.I_R1 :=
			(exp(-((state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*t)/(2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2))*(((2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*(state.fixed.L_L1*state.fixed.V_V1-state.init_I_L1*state.fixed.L_L1*state.fixed.R_R2)-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R2*(state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1))*sin((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2)))/(sqrt(state.fixed.L_L1)*sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0))+state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R2*cos((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2))))/(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2);

		rval.V_C1 :=
			(exp(-((state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*t)/(2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2))*(((2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*(state.fixed.L_L1*state.fixed.R_R1*state.fixed.V_V1-state.init_I_L1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2)-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*(state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1))*sin((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2)))/(sqrt(state.fixed.L_L1)*sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0))+state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*cos((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2))))/(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2);

		rval.I_C1 :=
			(exp(-((state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*t)/(2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2))*(((-(state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.V_V1+(-state.init_I_L1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1)*state.fixed.R_R2-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1)-2.0*state.init_V_C1*state.fixed.C_C1**2.0*state.fixed.L_L1*state.fixed.R_R1**2.0*state.fixed.R_R2**2.0)*sin((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2)))/(sqrt(state.fixed.L_L1)*sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0))+(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.V_V1+(-state.init_I_L1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1)*state.fixed.R_R2-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1)*cos((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2))))/(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2);

		rval.V_L1 :=
			(exp(-((state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*t)/(2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2))*(((2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*(state.fixed.L_L1*state.fixed.R_R1*state.fixed.V_V1-state.init_I_L1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2)-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*(state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1))*sin((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2)))/(sqrt(state.fixed.L_L1)*sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0))+state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*cos((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2))))/(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2);

		rval.I_L1 :=
			(exp(-((state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*t)/(2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2))*(((-2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*((state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*state.fixed.V_V1+(-state.init_V_C1*state.fixed.C_C1*state.fixed.R_R1-state.init_I_L1*state.fixed.L_L1)*state.fixed.R_R2**2.0-state.init_I_L1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2)-(state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*(state.init_I_L1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.V_V1))*sin((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2)))/(sqrt(state.fixed.L_L1)*sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0))+(state.init_I_L1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.V_V1)*cos((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2))))/(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2)+state.fixed.V_V1/state.fixed.R_R2;
		
		rval.V_R2 :=
			(exp(-((state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*t)/(2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2))*(((2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*(state.init_I_L1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1*state.fixed.V_V1)+state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*(state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1))*sin((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2)))/(sqrt(state.fixed.L_L1)*sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0))-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2*cos((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2))))/(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2)+state.fixed.V_V1;
		
		rval.I_R2 :=
			(exp(-((state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)*t)/(2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2))*(((state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*(state.fixed.L_L1*state.fixed.R_R2+state.fixed.L_L1*state.fixed.R_R1)-2.0*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*(state.fixed.L_L1*state.fixed.R_R1*state.fixed.V_V1-state.init_I_L1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2))*sin((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2)))/(sqrt(state.fixed.L_L1)*sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0))-state.init_V_C1*state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*cos((sqrt((4.0*state.fixed.C_C1*state.fixed.R_R1**2.0-state.fixed.L_L1)*state.fixed.R_R2**2.0-2.0*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2-state.fixed.L_L1*state.fixed.R_R1**2.0)*t)/(2.0*state.fixed.C_C1*sqrt(state.fixed.L_L1)*state.fixed.R_R1*state.fixed.R_R2))))/(state.fixed.C_C1*state.fixed.L_L1*state.fixed.R_R1*state.fixed.R_R2)+state.fixed.V_V1/state.fixed.R_R2;
		
		-- Save the new V_C1 and I_L1 as new initial values.
		rval.init_V_C1 := rval.V_C1;
		rval.init_I_L1 := rval.I_L1;
		
		return rval;
	end;
end;
