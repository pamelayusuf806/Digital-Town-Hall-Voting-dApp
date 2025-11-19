import { describe, expect, it } from "vitest";

const accounts = simnet.getAccounts();
const address1 = accounts.get("wallet_1")!;

/*
 * Governance Analytics and Reporting System Tests
 * Basic validation of new governance analytics features
 */

describe("Governance Analytics System", () => {
  it("governance summary function exists and is callable", () => {
    expect(simnet.blockHeight).toBeDefined();
    
    // Test that our new analytics functions are accessible
    const { result } = simnet.callReadOnlyFn(
      "Digital-Town-Hall-Voting-dApp",
      "get-governance-summary", 
      [],
      address1
    );
    
    expect(result).toBeDefined();
  });

  it("governance health metrics function is accessible", () => {
    const { result } = simnet.callReadOnlyFn(
      "Digital-Town-Hall-Voting-dApp",
      "get-governance-health-metrics",
      [],
      address1
    );
    
    expect(result).toBeDefined();
  });
});
