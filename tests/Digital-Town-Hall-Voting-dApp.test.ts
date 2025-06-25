import { assertEquals, runScenario } from '@stacks/clarity-js-sdk';
import { Chain } from '@stacks/clarity-js-sdk';
import { describe, it, beforeEach } from 'mocha';

describe('Digital Town Hall Voting Tests', () => {
  const sender = 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM';
  const voter1 = 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG';

  beforeEach(async () => {
    await runScenario(async (chain: Chain) => {
      await chain.mineBlock([]);
    });
  });

  it('should create a new proposal', async () => {
    const result = await runScenario(async (chain: Chain) => {
      const tx = chain.createProposal({
        sender,
        title: 'Test Proposal',
        description: 'Test Description',
        duration: 100
      });
      return tx;
    });
    assertEquals(result.success, true);
  });

  it('should allow voting on active proposal', async () => {
    const result = await runScenario(async (chain: Chain) => {
      const vote = chain.castVote({
        sender: voter1,
        proposalId: 1,
        vote: true
      });
      return vote;
    });
    assertEquals(result.success, true);
  });

  it('should prevent double voting', async () => {
    const result = await runScenario(async (chain: Chain) => {
      await chain.castVote({
        sender: voter1,
        proposalId: 1,
        vote: true
      });
      const secondVote = chain.castVote({
        sender: voter1,
        proposalId: 1,
        vote: true
      });
      return secondVote;
    });
    assertEquals(result.success, false);
  });
});