#lang datalog
friend(arnold,arnold).
student(arnold,arnold,arnold) :- friend(arnold,arnold).
student(arnold,arnold,arnold)?
