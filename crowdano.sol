pragma solidity ^0.7.0;

contract Crowdano {

}

contract Campaign {
    uint public id;
    uint public goal;
    uint public deadline
    address payable public owner;
    mapping(address => uint) public pledgeAmounts;
    uint public pendingWithdrawl;
    bool public failed;

    constructor(uint _id, uint _goal, uint _deadline, address payable _owner) public {
        id = _id;
        goal = _goal;
        deadline = _deadline;
        owner = _owner;
        failed = false;
        pendingWithdrawal = 0;
    }

    function support() public payable {
        require(block.timestamp < deadline);
        pledgeAmounts[msg.sender] = msg.value;
    }

    function checkFailed() public {
        require(block.timestamp >= deadline);
        if(this.balance < goal) {
            failed = true;
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
            msg.sender.trabsfer(amount);
        }
    }
}
