      *================================================================*
      * CodeUChain COBOL Library - Public Interface                  *
      *                                                                *
      * This file defines the public API for the CodeUChain COBOL     *
      * library. Include this file in your programs to use the        *
      * library functionality.                                        *
      *================================================================*

      *================================================================*
      * DATA STRUCTURES                                               *
      *================================================================*

      * Link Name Structure (used by all link operations)
       01  LINK-NAME.
           05  LINK-NAME-LEN       PIC S9(4) COMP.
           05  LINK-NAME-DATA      PIC X(30).

      * Chain Name Structure (used by chain operations)
       01  CHAIN-NAME.
           05  CHAIN-NAME-LEN      PIC S9(4) COMP.
           05  CHAIN-NAME-DATA     PIC X(30).

      * Middleware Name Structure (used by middleware operations)
       01  MIDDLEWARE-NAME.
           05  MIDDLEWARE-NAME-LEN PIC S9(4) COMP.
           05  MIDDLEWARE-NAME-DATA PIC X(30).

      * Operation Structure (used by middleware operations)
       01  OPERATION.
           05  OPERATION-LEN       PIC S9(4) COMP.
           05  OPERATION-DATA      PIC X(20).

      * Context Data (large buffer for passing data between components)
       01  CONTEXT-DATA            PIC X(10000).

      * Result Status (standard result codes)
       01  RESULT-STATUS           PIC X(10).

      *================================================================*
      * PUBLIC API PROCEDURES                                         *
      *================================================================*

      * Link Interface Procedures
      * CALL "LINK-INTERFACE" USING LINK-NAME, CONTEXT-DATA, CONTEXT-DATA, RESULT-STATUS

      * Chain Orchestrator Procedures
      * CALL "CHAIN-ORCHESTRATOR" USING CHAIN-NAME, CONTEXT-DATA, CONTEXT-DATA, RESULT-STATUS

      * Financial Calculator Procedures
      * CALL "FINANCIAL-CALCULATOR" USING LINK-NAME, CONTEXT-DATA, CONTEXT-DATA, RESULT-STATUS

      * Middleware Procedures
      * CALL "MIDDLEWARE-INTERFACE" USING MIDDLEWARE-NAME, CONTEXT-DATA, OPERATION, RESULT-STATUS
      * CALL "LOGGING-MIDDLEWARE" USING MIDDLEWARE-NAME, CONTEXT-DATA, OPERATION, RESULT-STATUS

      *================================================================*
      * USAGE EXAMPLES                                                *
      *================================================================*

      * Example: Simple Link Processing
      *     MOVE 11 TO LINK-NAME-LEN
      *     MOVE "MY-LINK" TO LINK-NAME-DATA
      *     CALL "LINK-INTERFACE" USING
      *         LINK-NAME, INPUT-DATA, OUTPUT-DATA, RESULT

      * Example: Chain Orchestration
      *     MOVE 12 TO CHAIN-NAME-LEN
      *     MOVE "MY-CHAIN" TO CHAIN-NAME-DATA
      *     CALL "CHAIN-ORCHESTRATOR" USING
      *         CHAIN-NAME, INPUT-DATA, OUTPUT-DATA, RESULT

      *================================================================*
      * VERSION INFORMATION                                           *
      *================================================================*

      * Library Version: 1.0.0
      * Compatible with: GnuCOBOL 3.0+
      * Last Updated: September 2025