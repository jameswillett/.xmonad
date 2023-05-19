#!/bin/sh

echo $1

if [[ $1 =~ ^sudo.* ]]
then
  gksu $1
else
  $1
fi
