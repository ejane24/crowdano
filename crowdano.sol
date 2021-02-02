//License: CC BY-NC-SA

//This smart-contract is not complete. 
//All functions are subject to change.

pragma solidity ^0.7.0;

contract Crowdano {

}

contract Campaign {
    uint public id;
    uint public goal;
    uint public deadline
    address payable public owner;
    uint initialFunding;
    bool initialFundingReceived;
    mapping(address => uint) public pledgeAmounts;
    mapping(address => uint) public votesCast;
    uint public pendingWithdrawal;
    bool public failed;
    bool public voteInProgress;
    uint public requestedAmount;
    uint public voteNumber;
    uint public voteScore;

    constructor(uint _id, uint _goal, uint _deadline, address payable _owner, uint _initialFunding) public {
        id = _id;
        goal = _goal;
        deadline = _deadline;
        owner = _owner;
        initialFunding = _initialFunding;
        initialFundingReceived = false;
        failed = false;
        pendingWithdrawal = 0;
        voteNumber = 0;
    }

    function support() public payable {
        require(block.timestamp < deadline);
        pledgeAmounts[msg.sender] = msg.value;
    }

    function checkFailed() public {
        require(block.timestamp >= deadline);
        require(!initialFundingReceived);
        if(this.balance < goal) {
            failed = true;
        } else {
            initialFundingReceived = true;
            pendingWithdrawal = initialFunding;
        }
    }

    function withdraw() public {
        if(failed) {
            uint amount = pledgeAmounts[msg.sender],
            pledgeAmounts[msg.sender] = 0;
            msg.sender.transfer(amount);
        } else if(msg.sender == owner) {
            uint amount = pendingWithdrawal;
            pendingWithdrawal = 0;
            msg.sender.transfer(amount);
        }
    }

    function requestFunds(uint amount) public {
        require(!voteInProgress);
        require(amount < this.balance);
        voteNumber += 1;
        voteInProgress = true;
        requestedFunds = amount;
        voteScore = 0;
    }

    function castVote(bool approve) {
        require(voteInProgress);
        require(pledgeAmounts[msg.sender] != 0);
        require(votesCast[address] < voteNumber);
        votesCast[address] += 1;
        if(approve) {
            voteScore += pledgeAmounts[msg.sender];
        } else {
            voteScore -= pledgeAmounts[msg.sender];
        }
    }
}
