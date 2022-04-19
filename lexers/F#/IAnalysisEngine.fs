//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

namespace Microsoft.Practices.ParallelGuideSamples.ADash

open System
open Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects

/// Represents an engine that performs the analysis
/// (analysis can be performed sequentially, in parallel using tasks
/// and also in parallel using non-blocking I/O using F# asyncs)
type IAnalysisEngine =
    
    /// Creates a process that represents sequential analysis. It can be used to
    /// register notifications when result becomes available and then 
    /// started using the 'Start' method or canceled using 'TryCancel'
    abstract DoSequentialAnalysis : unit -> AnalysisProcess

    /// Creates a process that represents parallel analysis. It can be used to
    /// register notifications when result becomes available and then 
    /// started using the 'Start' method or canceled using 'TryCancel'
    abstract DoAsyncAnalysis : unit -> AnalysisProcess
