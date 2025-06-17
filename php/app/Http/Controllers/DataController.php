<?php

namespace App\Http\Controllers;

use App\Exceptions\DynamoKeyNotFoundException;
use App\Services\DataService;
use Illuminate\Http\JsonResponse;
use Illuminate\Http\Request;
use Throwable;

/**
 * Summary of DataController
 */
class DataController extends Controller
{
    /**
     * Summary of __construct
     * @param \App\Services\DataService $dataService
     */
    public function __construct(
        private DataService $dataService
    ){}

    /**
     * Summary of get
     * @param string $key
     * @return JsonResponse|mixed
     */
    public function get(string $key): JsonResponse
    {
        try {
            $data = $this->dataService->get(key: $key);
            return response()->json(data: ['error' => 'ok', 'value' => $data]);
        } catch (Throwable $e) {
            if ($e instanceof DynamoKeyNotFoundException) {
                return response()->json(data: ['error' => 'not_found', 'value' => null]);
            } else {
                return response()->json(data: ['error' => 'internal', 'value' => null], status: 500);
            }
        }
    }

    /**
     * Summary of set
     * @param \Illuminate\Http\Request $request
     * @return JsonResponse|mixed
     */
    public function set(Request $request): JsonResponse
    {
        $validated = $request->validate(rules: [
            'key' => 'required|string',
            'value' => 'required|string'
        ]);

        try {
            $this->dataService->set(attributes: $validated);
            return response()->json(data: ['error' => 'ok']);
        } catch (Throwable) {
            return response()->json(data: ['error' => 'internal'], status: 500);
        }
    }
}
