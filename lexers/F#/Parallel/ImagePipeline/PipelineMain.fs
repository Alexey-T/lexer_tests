//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

module Microsoft.Practices.ParallelGuideSamples.ImagePipeline.Pipeline

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Drawing
open System.IO
open System.Linq
open System.Threading
open System.Threading.Tasks

open FSharp.Control
open Microsoft.Practices.ParallelGuideSamples.Utilities
open Microsoft.Practices.ParallelGuideSamples.ImagePipeline.Extensions

// ------------------------------------------------------------------------------
// Image Pipeline Top Level Loop
// ------------------------------------------------------------------------------

type ImageMode =
    | Sequential = 0
    | Pipelined = 1
    | LoadBalanced = 2
    | MessagePassing = 3

let QueueBoundedCapacity = 4
let LoadBalancingDegreeOfConcurrency = 2
let MaxNumberOfImages = 500

/// <summary>
///   Runs the image pipeline example. The program goes through the jpg images located in the SourceDir
///   directory and performs a series of steps: it resizes each image and adds a black border and then applies
///   a Gaussian noise filter operation to give the image a grainy effect. Finally, the program invokes 
///   a user-provided delegate to the image (for example, to display the image on the user interface).
/// 
///   Images are processed in sequential order. That is, the display delegate will be 
///   invoked in exactly the same order as the images appear in the file system.
/// </summary>
///
/// <param name="displayFn">
///   A function that is invoked for each image at the end of the pipeline, 
///   for example, to display the image in the user interface.
/// </param>
/// <param name="algorithmChoice">
///   The method of calculation. 0=sequential, 1=pipeline, 2=load balanced pipeline
/// </param>
/// <param name="errorFn">
///   A function that will be invoked if this method or any of its parallel 
///   subtasks observe an exception during their execution.
/// </param>
/// <param name="token">A token that can signal an external cancellation request.</param>
[<SuppressMessage("Microsoft.Design", "CA1031:DoNotCatchGeneralExceptionTypes")>]
let imagePipelineMainLoop displayFn token algorithmChoice errorFn =
    try
        let sourceDir = Directory.GetCurrentDirectory()
        // Ensure that frames are presented in sequence before invoking the user-provided display function.
        let imagesSoFar = ref 0
        let safeDisplayFn (info:ImageInfo) =
            if info.SequenceNumber <> !imagesSoFar then
                failwithf "Images processed out of order. Saw %d, expected %d" info.SequenceNumber (!imagesSoFar)
            displayFn info
            incr imagesSoFar

        // Create a cancellation handle for inter-task signaling of exceptions. This cancellation
        // handle is also triggered by the incoming token that indicates user-requested
        // cancellation.
        use cts = CancellationTokenSource.CreateLinkedTokenSource([| token |]) 
        let fileNames = SampleUtilities.GetImageFilenames sourceDir MaxNumberOfImages
        match algorithmChoice with
        | ImageMode.Sequential -> PipelineStandard.runSequential fileNames sourceDir safeDisplayFn cts
        | ImageMode.Pipelined -> PipelineStandard.runPipelined fileNames sourceDir QueueBoundedCapacity safeDisplayFn cts
        | ImageMode.LoadBalanced -> PipelineStandard.runLoadBalancedPipeline fileNames sourceDir QueueBoundedCapacity safeDisplayFn cts LoadBalancingDegreeOfConcurrency
        | ImageMode.MessagePassing -> PipelineAgent.runMessagePassing fileNames sourceDir QueueBoundedCapacity safeDisplayFn cts
        | _ -> failwith "invalid case"
    with 
    | :? AggregateException as ae when ae.InnerExceptions.Count = 1 ->
        errorFn (ae.InnerExceptions.[0])
    | :? OperationCanceledException as e -> reraise()
    | e -> errorFn e
