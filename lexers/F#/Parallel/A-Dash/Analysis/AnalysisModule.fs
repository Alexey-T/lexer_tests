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
open System.Collections.Generic
open System.Linq
open System.Threading
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects
open Microsoft.Practices.ParallelGuideSamples.Utilities

// ------------------------------------------------------------------------------
// Module that implements data analysis operations and additional
// helper methods that are used in tha async workflow implementation
// ------------------------------------------------------------------------------

module AnalysisModule =

    let mutable speedFactor = 1.0

    // --------------------------------------------------------------------------
    // Generate sample data for analysis
    // --------------------------------------------------------------------------

    // Generate sample data for market analysis
    let generateSecurities exchange size =
      new ResizeArray<_>
        ([ for i in 0 .. size - 1 do
              yield { name = exchange + " Stock " + string i
                      priceHistory = [ 0.0; 1.0; 2.0 ] } ])

    let makeNyseSecurityInfo() =
        generateSecurities "NYSE" 100

    let makeNasdaqSecurityInfo() =
        generateSecurities "NASDAQ" 100

    let makeFedSecurityInfo() =
        generateSecurities "" 100

    // Synchronous I/O intensive analysis methods

    let loadNyseData() =
        SampleUtilities.DoIoIntensiveOperationSimple 2.5 |> ignore 
        new StockDataCollection(makeNyseSecurityInfo())

    let loadNasdaqData() =
        SampleUtilities.DoIoIntensiveOperationSimple (2.0 * speedFactor) |> ignore
        new StockDataCollection(makeNasdaqSecurityInfo())

    let loadFedHistoricalData() = 
        SampleUtilities.DoIoIntensiveOperationSimple (3.0 * speedFactor) |> ignore
        new StockDataCollection(makeFedSecurityInfo())

    // Asynchronous I/O intensive analysis methods (F# specific)

    let asyncLoadNyseData() = async {
        let! _ = SampleUtilities.AsyncDoIoIntensiveOperationSimple 2.5 
        return new StockDataCollection(makeNyseSecurityInfo()) }

    let asyncLoadNasdaqData() = async {
        let! _ = SampleUtilities.AsyncDoIoIntensiveOperationSimple (2.0 * speedFactor) 
        return new StockDataCollection(makeNasdaqSecurityInfo()) }

    let asyncLoadFedHistoricalData() = async {
        let! _ = SampleUtilities.AsyncDoIoIntensiveOperationSimple (3.0 * speedFactor) 
        return new StockDataCollection(makeFedSecurityInfo()) }

    // --------------------------------------------------------------------------
    // CPU intensive analysis methods
    // --------------------------------------------------------------------------

    let mergeMarketData allMarketData =
        SampleUtilities.DoCpuIntensiveOperationSimple (2.0 * speedFactor) |> ignore
        new StockDataCollection(new ResizeArray<_>(Seq.concat allMarketData))

    let normalizeData marketData =
        SampleUtilities.DoCpuIntensiveOperationSimple (2.0 * speedFactor) |> ignore
        new StockDataCollection(marketData)

    let analyzeData data = 
        MarketAnalyzer.Run(data)

    let runModel data =
        SampleUtilities.DoCpuIntensiveOperationSimple (2.0 * speedFactor) |> ignore
        MarketModeler.Run(data)

    let compareModels models =
        SampleUtilities.DoCpuIntensiveOperationSimple (2.0 * speedFactor) |> ignore
        ModelComparer.Run(models |> Array.ofSeq)

    // --------------------------------------------------------------------------
    // Asynchronous versions of CPU intensive analysis methods
    // (Simply wrap the call to enable cooperative cancellation)
    // --------------------------------------------------------------------------

    let asyncMergeMarketData allMarketData =
        async { return mergeMarketData allMarketData }

    let asyncNormalizeData marketData =
        async { return normalizeData marketData }

    let asyncAnalyzeData data = 
        async { return analyzeData data }

    let asyncRunModel data =
        async { return runModel data }

    let asyncCompareModels models =
        async { return compareModels models }