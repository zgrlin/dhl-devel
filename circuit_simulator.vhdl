library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.circuit_math_pkg.all;


entity circuit_simulator is
end;

architecture tb of circuit_simulator is
	function fixed_init_val return fixed_values_t is
		variable r: fixed_values_t;
	begin
		r := (others => 0.0);
		
		r.V_V1 := 0.0; -- Here you can enter other value.
		r.R_R1 := 270.0; -- Here you can enter other value.
		r.R_R2 := 100.0; -- Here you can enter other value.
		r.C_C1 := 33.0e-6; -- Here you can enter other value.
		r.L_L1 := 10.0e-3; -- Here you can enter other value.
		
		return r;
	end;
	
	function circuit_init_val return circuit_state_t is
		variable r: circuit_state_t;
	begin
		r := (timestamp => 0 ns, fixed => fixed_init_val, others => 0.0);
		
		r.init_V_C1 := 0.0; -- Here you can enter other initial value.
		r.init_I_L1 := 0.0; -- Here you can enter other initial value.
		return r;
	end;

	signal circuit_state: circuit_state_t := circuit_init_val;
	signal fixed_state: fixed_values_t := fixed_init_val;
	
	signal adc_clk: std_logic;
	signal adc_analog_input_value: real := 0.0;
begin
	circuit_evaluator_pr: process(fixed_state)
		variable new_circuit_state: circuit_state_t;
	begin
		new_circuit_state := circuit_evaluated(circuit_state, now);
		new_circuit_state.fixed := fixed_state;
		circuit_state <= new_circuit_state;
	end process;
	
	input_waveform_pr: process
	begin
		fixed_state <= fixed_init_val;
		
		wait for 1 ms;
		fixed_state.V_V1 <= 10.0;
		
		wait for 100 ms;
		fixed_state.V_V1 <= 0.0;
		
		wait for 99 ms;
		fixed_state.V_V1 <= 0.0;
		
		wait;
	end process;
	
	adc_clk_pr: process
	begin
		adc_clk <= '0';
		wait for 15 us;
		adc_clk <= '1';
		wait for 15 us;
	end process;
	
	adc_evaluate_pr: process(adc_clk)
		variable eval_state: circuit_state_t;
	begin
		if rising_edge(adc_clk) then
			eval_state := circuit_evaluated(circuit_state, now);
			adc_analog_input_value <= eval_state.V_R2;
		end if;
	end process;
end;
