# Digital Town Hall Voting dApp
A decentralized voting application for cities and schools built on Stacks blockchain.

## 🚀 Features

- Create and manage proposals
- Secure voting mechanism
- Real-time results tracking
- Access control and validation
- Block height-based voting periods

## 🛠️ Technical Details

### Security Features
- Owner-only proposal creation
- Single vote per wallet
- Timebound voting periods
- Input validation

### Optimizations
- Efficient data structures
- Minimal storage usage
- Optimized vote counting

## 📋 Usage Instructions

1. Deploy contract:
```bash
clarinet deploy
```

2. Create proposal:
```bash
clarinet contract-call --contract-address ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM --function create-proposal
```

3. Cast vote:
```bash
clarinet contract-call --contract-address ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM --function cast-vote
```

## 🧪 Testing

Run tests:
```bash
clarinet test
```

## 🎨 UI Components

- Proposal Creation Form
- Active Proposals Dashboard
- Voting Interface
- Results Visualization
- Voter History Page
