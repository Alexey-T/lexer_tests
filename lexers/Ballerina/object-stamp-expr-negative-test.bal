// Copyright (c) 2018 WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
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

type PersonObj object {
    public int age = 10;
    public string name = "mohan";

    public int year = 2014;
    public string month = "february";
};

type EmployeeObj object {
    public int age = 10;
    public string name = "raj";

};

type Employee record {
    string name;
    int age;
};

type EmployeeObject object {
    string name = "Mohan";
    string status = "Single";
    string batch = "LK2014";
};

type IntObject object{
    string firstName = "Mohan";
    string lastName = "Raj";
};

type TeacherObj object {
    string name = "Mohan";
    int age = 30;
    string status = "Single";
    string batch = "LK2014";
    string school = "VNC";
};

type BookObject object {
    string book = "XYZ";
};


type ExtendedEmployee record {
    string name;
    string status;
    string batch;
    Address address;
};

type Address object {
    public int no = 10;
    public string streetName = "Palm Grove";
    public string city = "colombo";
};

type Teacher record {
    string name;
    int age;
    string status;
    string batch;
    string school;
};

//----------------------------Object Stamp Negative Test Cases -------------------------------------------------------------

function stampObjectsToRecord() returns Employee|error {
    PersonObj p = new PersonObj();
    Employee|error employee = Employee.constructFrom(p);

    return employee;
}

function stampObjectsToJSON() returns json|error {
    PersonObj p = new PersonObj();
    json|error jsonValue = json.constructFrom(p);

    return jsonValue;
}

function stampObjectsToXML() returns xml|error {
    PersonObj p = new PersonObj();
    xml|error xmlValue = xml.constructFrom(p);

    return xmlValue;
}

function stampObjectsToMap() returns map<any>|error {
    PersonObj p = new PersonObj();
    map<any>|error mapValue = map<any>.constructFrom(p);

    return mapValue;
}

function stampObjectsToArray() returns any[]|error {
    PersonObj p = new PersonObj();
    any[]|error anyValue = any[].constructFrom(p);

    return anyValue;
}

function stampObjectsToTuple() returns [int,string]|error {
    PersonObj p = new PersonObj();
    [int, string]|error tupleValue = [int,string].constructFrom(p);

    return tupleValue;
}

function stampAnyToObject() returns PersonObj|error {

    anydata anydataValue = new PersonObj();
    PersonObj|error personObj = PersonObj.constructFrom(anydataValue);

    return personObj;
}

function stampAnyArrayToObject() returns EmployeeObject|error {

    anydata[] anyArray = ["Mohan", "Single", "LK2014"];
    EmployeeObject|error objectValue = EmployeeObject.constructFrom(anyArray);

    return objectValue;
}

function stampJSONToObject() returns EmployeeObj|error {

    json employee = { name: "John", status: "Single", batch: "LK2014" };
    EmployeeObject|error employeeObj = EmployeeObject.constructFrom(employee);
    return employeeObj;
}

function stampXMLToObject() returns BookObject|error {

    xml xmlValue = xml `<book>The Lost World</book>`;

    BookObject|error objectValue = BookObject.constructFrom(xmlValue);
    return objectValue;
}

function stampMapToObject() returns IntObject|error {
    map<anydata> m = { "firstName": "mohan", "lastName": "raj" };
    IntObject|error objectValue = IntObject.constructFrom(m);

    return objectValue;
}

function stampRecordToObject() returns TeacherObj|error {

    Teacher teacher = { name: "Raja", age: 25, status: "single", batch: "LK2014", school: "Hindu College" };
    TeacherObj|error returnValue = TeacherObj.constructFrom(teacher);

    return returnValue;
}

function stampTupleToObject() returns EmployeeObj|error {
    [string, int] tupleValue = ["Mohan", 30];

    EmployeeObj|error objectValue = EmployeeObj.constructFrom(tupleValue);
    return objectValue;
}
