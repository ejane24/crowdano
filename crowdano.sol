pragma solidity ^0.7.0;

contract Crowdano {

}

contract Campaign {
    uint public id;
    uint public goal;
    uint public deadline
    address payable public owner;
    mapping(address => uint) public pledgeAmounts

    constructor(uint _id, uint _goal, uint _deadline, address payable _owner) public {
        id = _id;
        goal = _goal;
        deadline = _deadline;
        owner = _owner;
    }

    function support() public payable {
        require(block.timestamp < deadline);
        pledgeAmounts[msg.sender] = msg.value;
    }


}
