//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================
#nowarn "40"

namespace Microsoft.Practices.ParallelGuideSamples.ADash

open System
open System.Collections.Generic
open System.Linq
open System.Threading
open System.Threading.Tasks
open Microsoft.Practices.ParallelGuideSamples.ADash.BusinessObjects
open Microsoft.Practices.ParallelGuideSamples.Utilities

// Module that contains basic analysis operations
open Microsoft.Practices.ParallelGuideSamples.ADash.AnalysisModule

// Extension methods for 'Async'
open FSharp.Control

// --------------------------------------------------------------------------
// Sequential implementation of the analysis engine
// --------------------------------------------------------------------------

type AnalysisEngine() =    
    interface IAnalysisEngine with

        /// Creates a market recommendation using a fully sequential operation
        member x.DoSequentialAnalysis() =

            // Analysis engine that will be returned as the result.
            // 'Start' runs the analysis function below & 'TryCancel' is not supported
            let rec engine = 
              { new AnalysisProcess() with
                  member x.Start() = Task.Factory.StartNew(analysis) |> ignore
                  member x.TryCancel() = () }


            // Function that implements the sequential analysis
            and analysis() =
                // Load & process NYSE and NASDAQ data
                let nyseData = loadNyseData()
                engine.TriggerLoadNyseData(nyseData)
                let nasdaqData = loadNasdaqData()
                engine.TriggerLoadNasdaqData(nasdaqData)

                let mergedMarketData = mergeMarketData [ nyseData; nasdaqData ]
                engine.TriggerMergeMarketData(mergedMarketData)
                let normalizedMarketData = normalizeData mergedMarketData
                engine.TriggerNormalizeMarketData(normalizedMarketData)
                let analyzedStockData = analyzeData normalizedMarketData
                engine.TriggerAnalyzeMarketData(analyzedStockData)
            
                // Load & process FED data
                let fedHistoricalData = loadFedHistoricalData()
                engine.TriggerLoadFedHistoricalData(fedHistoricalData)
                let normalizedHistoricalData = normalizeData fedHistoricalData
                engine.TriggerNormalizeHistoricalData(normalizedHistoricalData)
                let analyzedHistoricalData = analyzeData normalizedHistoricalData
                engine.TriggerAnalyzeHistoricalData(analyzedHistoricalData)
            
                // Run both models and compare them
                let modeledMarketData = runModel analyzedStockData
                engine.TriggerModelMarketData(modeledMarketData)
                let modeledHistoricalData = runModel analyzedHistoricalData
                engine.TriggerModelHistoricalData(modeledHistoricalData)
                let recommendation = compareModels [ modeledMarketData; modeledHistoricalData ]
                engine.TriggerCompareModels(Some recommendation)
            engine


        /// Initiates market analysis using asynchronous workflows (F# async).
        /// Returns an object that can be used to register notifications with 
        /// the background calculations (and can be used to get the results)
        ///
        /// To notify the caller, we create events that are triggered (on the
        /// GUI thread) when asynchronous workflow produces partial result.
        /// The returned object registers handlers with the events.
        member x.DoAsyncAnalysis() =
            let cts = new CancellationTokenSource()

            // Analysis engine that will be returned as the result.
            // 'Start' runs the workflows & 'TryCancel' sets cancellation token
            let rec engine = 
              { new AnalysisProcess() with
                  member x.Start() = 
                      // Start workflow with cancellation token
                      Async.Start(compare, cancellationToken = cts.Token)
                  member x.TryCancel() = cts.Cancel() }

            // Load & process NYSE and NASDAQ data
            and marketModel = async {
                // Start loading of data from two sources in background (both I/O
                // operations are perfomed asynchronously without blocking threads)
                let! nyse = 
                  asyncLoadNyseData() 
                  |> Async.WithResult engine.TriggerLoadNyseData |> Async.StartChild
                let! nasdaq = 
                  asyncLoadNasdaqData() 
                  |> Async.WithResult engine.TriggerLoadNasdaqData |> Async.StartChild
                
                // Wait for both tasks to complete and continue
                let! nyseData = nyse
                let! nasdaqData = nasdaq
                let merged = mergeMarketData [ nyseData; nasdaqData ] 
                engine.TriggerMergeMarketData(merged)
                
                // Perform analysis of the merged data
                let! normalized = asyncNormalizeData merged
                engine.TriggerNormalizeMarketData(normalized)
                let! analyzed = asyncAnalyzeData normalized 
                engine.TriggerAnalyzeMarketData(analyzed)
                let! res = asyncRunModel analyzed 
                engine.TriggerModelMarketData(res)
                return res }

            // Load & process FED data
            and historicalModel = async {
                // Obtain data asynchronously using non-blocking I/O
                let! fed = asyncLoadFedHistoricalData()
                engine.TriggerLoadFedHistoricalData(fed)

                // Perform CPU-intensive analysis of the data
                let! normalized = asyncNormalizeData fed
                engine.TriggerNormalizeHistoricalData(normalized)
                let! analyzed = asyncAnalyzeData normalized
                engine.TriggerAnalyzeHistoricalData(analyzed)
                let! res = asyncRunModel analyzed 
                engine.TriggerModelHistoricalData(res)
                return res }

            // Run both of the models and compare them to get recommendation
            // When error occurs, it is propagated here and handled using 'try'
            and compare = 
              Async.TryCancelled
                (async {
                  try
                      let! models = Async.Parallel [ marketModel; historicalModel ]
                      let res = compareModels models 
                      engine.TriggerCompareModels(Some res)
                  with e ->
                      engine.TriggerErrorHandler()  },
                  fun ce -> 
                      engine.TriggerCompareModels(None))

            // Return the created engine
            engine