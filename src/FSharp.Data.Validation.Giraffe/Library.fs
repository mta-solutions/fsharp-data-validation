namespace FSharp.Data.Validation.Giraffe

open Giraffe

[<AutoOpen>]
module ModelValidation =

    /// <summary>
    /// Interface defining model validation methods.
    ///
    /// This interface is a re-implementation of the Giraffe interface of the same
    /// name, but with the addition of a generic type parameter 'U which is the
    /// type of the object returned by the Validate method.
    /// </summary>
    type IModelValidation<'T, 'U> =
        /// <summary>
        /// Contract for validating an object's state.
        ///
        /// If the object has a valid state then the function should return the object, otherwise it should return a `HttpHandler` function which is ought to return an error response back to a client.
        /// </summary>
        abstract member Validate : unit -> Result<'U, HttpHandler>

    /// <summary>
    /// Validates an object of type 'T where 'T must have implemented interface <see cref="IModelValidation{T, U}"/>.
    ///
    /// If validation was successful then object 'T will be passed into the <see cref="HttpHandler"/> function "f", otherwise an error response will be sent back to the client.
    /// </summary>
    /// <param name="f">A function which accepts the model 'T and returns a <see cref="HttpHandler"/> function.</param>
    /// <param name="model">An instance of type 'T, where 'T must implement interface <see cref="IModelValidation{T, U}"/>.</param>
    /// <typeparam name="'T"></typeparam>
    /// <typeparam name="'U"></typeparam>
    /// <returns>A Giraffe <see cref="HttpHandler"/> function which can be composed into a bigger web application.</returns>
    let validateModel<'T, 'U when 'T :> IModelValidation<'T, 'U>> (f : 'U -> HttpHandler) (model : 'T) : HttpHandler =
        match model.Validate() with
        | Ok validatedModel -> f validatedModel
        | Error err -> err
