// Copyright (c) 2019 WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
//
// WSO2 Inc. licenses this file to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file except
// in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.

type Person record {
    string name;
};

function getTaintedParson() returns @tainted Person {
    return { name: "Arthur Dent" };
}

function sensitiveF(@untainted Person p) {
    // pass
}

function driver() {
    Person p = getTaintedParson();
    json jp = <json> json.constructFrom(p);
    Person sameP = <Person> Person.constructFrom(jp);
    sensitiveF(sameP);
    sensitiveF(sameP.cloneReadOnly());
}

function driver2() {
    Person p = getTaintedParson();
    sensitiveF(p.cloneReadOnly());
}
