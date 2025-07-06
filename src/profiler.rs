use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Profiling data collector for VM instructions
#[derive(Debug, Default)]
pub struct Profiler {
    pub enabled: bool,
    instruction_counts: HashMap<String, u64>,
    instruction_times: HashMap<String, Duration>,
    function_call_counts: HashMap<String, u64>,
}

impl Profiler {
    /// Create a new profiler
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Create a new profiler with enabled state
    pub fn new_with_enabled(enabled: bool) -> Self {
        Self {
            enabled,
            instruction_counts: HashMap::new(),
            instruction_times: HashMap::new(),
            function_call_counts: HashMap::new(),
        }
    }
    
    /// Enable profiling
    pub fn enable(&mut self) {
        self.enabled = true;
    }
    
    /// Disable profiling
    pub fn disable(&mut self) {
        self.enabled = false;
    }
    
    /// Record instruction execution
    pub fn record_instruction(&mut self, instruction_name: String, elapsed: Duration) {
        if !self.enabled {
            return;
        }
        
        // Update instruction counts
        *self.instruction_counts.entry(instruction_name.clone()).or_insert(0) += 1;
        
        // Update instruction times
        let total_time = self.instruction_times.entry(instruction_name).or_insert(Duration::new(0, 0));
        *total_time += elapsed;
    }
    
    /// Record function call
    pub fn record_function_call(&mut self, func_name: String) {
        if !self.enabled {
            return;
        }
        
        *self.function_call_counts.entry(func_name).or_insert(0) += 1;
    }
    
    /// Clear all profiling data
    pub fn clear(&mut self) {
        self.instruction_counts.clear();
        self.instruction_times.clear();
        self.function_call_counts.clear();
    }
    
    /// Generate profiling report
    pub fn generate_report(&self) -> String {
        if !self.enabled {
            return "Profiling is not enabled".to_string();
        }
        
        let mut output = String::new();
        output.push_str("=== VM Profiling Results ===\n\n");
        
        // Sort instructions by total time (descending)
        let mut time_sorted: Vec<_> = self.instruction_times.iter().collect();
        time_sorted.sort_by(|a, b| b.1.cmp(a.1));
        
        output.push_str("Instructions by total execution time:\n");
        output.push_str("Instruction        | Count      | Total Time  | Avg Time    \n");
        output.push_str("-------------------|------------|-------------|-------------\n");
        
        for (instruction, total_time) in time_sorted {
            let count = self.instruction_counts.get(instruction).unwrap_or(&0);
            let avg_time = if *count > 0 {
                total_time.as_nanos() / (*count as u128)
            } else {
                0
            };
            
            output.push_str(&format!(
                "{:<18} | {:>10} | {:>8}ms | {:>8}ns\n",
                instruction,
                count,
                total_time.as_millis(),
                avg_time
            ));
        }
        
        // Sort instructions by count (descending)
        let mut count_sorted: Vec<_> = self.instruction_counts.iter().collect();
        count_sorted.sort_by(|a, b| b.1.cmp(a.1));
        
        output.push_str("\nInstructions by execution count:\n");
        output.push_str("Instruction        | Count      | Total Time  \n");
        output.push_str("-------------------|------------|-------------\n");
        
        for (instruction, count) in count_sorted {
            let zero_duration = Duration::new(0, 0);
            let total_time = self.instruction_times.get(instruction).unwrap_or(&zero_duration);
            output.push_str(&format!(
                "{:<18} | {:>10} | {:>8}ms\n",
                instruction,
                count,
                total_time.as_millis()
            ));
        }
        
        // Function call statistics
        if !self.function_call_counts.is_empty() {
            output.push_str("\nFunction call statistics:\n");
            output.push_str("Function           | Call Count \n");
            output.push_str("-------------------|------------\n");
            
            let mut func_sorted: Vec<_> = self.function_call_counts.iter().collect();
            func_sorted.sort_by(|a, b| b.1.cmp(a.1));
            
            for (func_name, count) in func_sorted {
                output.push_str(&format!(
                    "{:<18} | {:>10}\n",
                    func_name,
                    count
                ));
            }
        }
        
        output
    }
    
    /// Save profiling report to file
    pub fn save_report_to_file(&self, filename: &str) -> Result<(), std::io::Error> {
        use std::fs::File;
        use std::io::Write;
        
        let report = self.generate_report();
        let mut file = File::create(filename)?;
        file.write_all(report.as_bytes())?;
        Ok(())
    }
}

/// Helper struct to time instruction execution
pub struct InstructionTimer {
    start_time: Option<Instant>,
}

impl InstructionTimer {
    /// Start timing if profiling is enabled
    pub fn start(enabled: bool) -> Self {
        Self {
            start_time: if enabled { Some(Instant::now()) } else { None },
        }
    }
    
    /// Finish timing and return elapsed duration if profiling was enabled
    pub fn finish(self) -> Option<Duration> {
        self.start_time.map(|start| start.elapsed())
    }
}