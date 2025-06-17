<?php

use App\Http\Controllers\DataController;
use Illuminate\Support\Facades\Route;

Route::get(uri: "get", action: [DataController::class, "get"])->name(name: "get");
Route::post(uri: "set", action: [DataController::class, "set"])->name(name: "set");
