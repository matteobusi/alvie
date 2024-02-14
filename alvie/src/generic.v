initial
   begin
      repeat(5) @(posedge mclk);
      stimulus_done = 0;

      // Initialize data memory to 0x0000
      for (tb_idx=0; tb_idx < `DMEM_SIZE/2; tb_idx=tb_idx+1)
        dmem_0.mem[tb_idx] = 16'h0000;

      // Initialize program memory - filled from OCaml code
      $readmemh("pmem.mem", pmem_0.mem);
      @(posedge mclk); // Here we should be at the beginning of the 6th cycle

      // @(posedge crypto_start);
      @(posedge exec_done); // Wait until RESET is complete!
      repeat(2) @(posedge mclk); // 2 more cycles are needed to complete the reset

      // if (!sm_0_enabled)
      //   tb_error("====== SM NOT ENABLED ======");

      // @(posedge sm_0_executing);

      // Wait until the end of the test or until a RESET is requested
      @(r15==16'hdead or negedge puc_rst);

      // Complete the stimulus
      stimulus_done = 1;

      // If a RESET was requested, wait for its completion
      @(r15==16'hdead or posedge exec_done);
      @(posedge mclk);

      // Forcefully set the r15 register to the "end of test" value
      dut.execution_unit_0.register_file_0.r15 = 16'hdead;
   end
